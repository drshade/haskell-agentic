-- | Plain-data transport for pending waits. This does not store, route or
-- authenticate waits. Keep the flow revision with the descriptor and use
-- 'Agentic.Interpret.fulfil' to validate a reply with the actual flow codec.
module Agentic.Wait (pendingWaitValue, pendingWaitFromValue) where

import Agentic.Core (Note (..))
import Agentic.Internal.Address
import Agentic.Internal.Replay (schemaValue)
import Agentic.Journal
import Agentic.Runtime (PendingWait (..))
import Agentic.Schema
import Agentic.Value
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | A versioned Value suitable for a caller-owned pending-wait store. No flow
-- action or model is needed to encode or decode it. Requests remain data and
-- may be sensitive; unlike summaryValue this is not a metadata-only view.
pendingWaitValue :: PendingWait -> Value
pendingWaitValue w = Object
  [("version",Integer 1)
  ,("identity",Object [("run",String w.identity.namespace),("revision",String w.identity.revision)])
  ,("key",durableKeyValue w.key),("request",w.request)
  ,("requestSchema",schemaValue w.requestSchema),("replySchema",schemaValue w.replySchema)
  ,("path",Array [Object [("name",String n.name),("description",maybe Null String n.description)] | n <- w.path])]

-- | Decode a trusted descriptor, checking its format, wait operation and
-- consistent run namespace. This is not validation of current reachability or
-- of the flow revision's code. 'Agentic.Interpret.fulfil' checks the destination
-- journal identity and resolves the actual reply codec from the supplied flow.
pendingWaitFromValue :: Value -> Either Text PendingWait
pendingWaitFromValue value = do
  v <- canonicalValue value
  fs <- object v
  version <- field "version" fs
  unless (version == Integer 1) (Left "unsupported pending-wait version")
  ident <- field "identity" fs >>= object
  identity <- JournalIdentity <$> textField "run" ident <*> textField "revision" ident
  keyObject <- field "key" fs >>= object
  namespace <- textField "run" keyObject
  unless (namespace == identity.namespace) (Left "wait namespace differs from identity")
  k <- field "key" keyObject >>= keyFromValue
  request <- field "request" fs
  input <- field "requestSchema" fs >>= schemaFromValue
  output <- field "replySchema" fs >>= schemaFromValue
  path <- field "path" fs >>= list note
  let pending = PendingWait identity (DurableKey namespace k) request input output path
  encoded <- canonicalValue (pendingWaitValue pending)
  unless (encoded == v) (Left "unexpected pending-wait fields")
  pure pending
 where
  note v = do
    fs <- object v
    Note <$> textField "name" fs <*> (field "description" fs >>= optional text)

keyFromValue :: Value -> Either Text JournalKey
keyFromValue v = do
  fs <- object v
  op <- field "operation" fs
  unless (op == Array [String "wait"]) (Left "key is not a wait")
  address <- textField "address" fs >>= addressFromText
  context <- field "context" fs >>= list frame
  pure (JournalKey AwaitedReply address context)
 where
  address v = text v >>= addressFromText
  frame = \case
    Array [String "each",a,i] -> ElementFrame <$> address a <*> index i
    Array [String "repeat",a,i] -> IterationFrame <$> address a <*> index i
    Array [String "call",a,t,i,c] -> InvocationFrame <$> address a <*> index t <*> index i <*> text c
    _ -> Left "invalid wait context frame"

addressFromText :: Text -> Either Text Address
addressFromText encoded = case T.splitOn "/" encoded of
  "flow":parts -> do
    a <- Address <$> segments parts
    unless (addressText a == encoded) (Left "noncanonical address")
    pure a
  _ -> Left "invalid address"
 where
  segments [] = Right []
  segments (tag:n:rest) | tag `elem` ["seq","fan","split","choose","tool","member"] = do
    i <- maybe (Left "invalid address index") index (Integer <$> readMaybe (T.unpack n))
    s <- case (tag,i) of
      ("seq",0) -> Right SeqLeft; ("seq",1) -> Right SeqRight
      ("fan",0) -> Right FanLeft; ("fan",1) -> Right FanRight
      ("split",0) -> Right SplitLeft; ("split",1) -> Right SplitRight
      ("choose",0) -> Right ChooseLeft; ("choose",1) -> Right ChooseRight
      ("tool",k) -> Right (ToolAt k); ("member",k) -> Right (MemberAt k)
      _ -> Left "invalid address segment"
    (s:) <$> segments rest
  segments (tag:rest) = do
    s <- case tag of
      "run" -> Right RunRoot; "call" -> Right CallScope
      "second" -> Right SecondBody; "repeat-test" -> Right RepeatTest
      "first" -> Right FirstBody; "each" -> Right EachBody
      "repeat" -> Right RepeatBody; "note" -> Right NotedBody
      _ -> Left "invalid address segment"
    (s:) <$> segments rest

schemaFromValue :: Value -> Either Text Schema
schemaFromValue v = do
  fs <- object v
  Schema <$> (field "title" fs >>= optional text)
    <*> (field "doc" fs >>= optional text)
    <*> (field "checks" fs >>= list text)
    <*> (field "shape" fs >>= shapeFromValue)
 where
  shapeFromValue v = do
    fs <- object v
    kind <- textField "kind" fs
    case kind of
      "object" -> SObject <$> (field "fields" fs >>= list fieldFromValue)
      "sum" -> SSum <$> (field "variants" fs >>= list variant)
      "enum" -> SEnum <$> (field "options" fs >>= list option)
      "array" -> SArray <$> (field "item" fs >>= schemaFromValue)
      "nullable" -> SNullable <$> (field "item" fs >>= schemaFromValue)
      "string" -> SString <$> (field "format" fs >>= optional format)
      "integer" -> Right SInteger; "number" -> Right SNumber
      "bool" -> Right SBool; "null" -> Right SNull
      _ -> Left "invalid schema shape"
  fieldFromValue v = do
    fs <- object v
    Field <$> textField "name" fs <*> (field "schema" fs >>= schemaFromValue)
      <*> (field "required" fs >>= boolean)
  variant v = do
    fs <- object v
    Variant <$> textField "tag" fs <*> (field "doc" fs >>= optional text)
      <*> (field "fields" fs >>= list fieldFromValue)
  option = \case
    Array [name,doc] -> (,) <$> text name <*> optional text doc
    _ -> Left "invalid enum option"
  format v = text v >>= \case
    "dateTime" -> Right DateTime; "date" -> Right Date; "email" -> Right Email
    "uri" -> Right Uri; "uuid" -> Right Uuid
    _ -> Left "invalid string format"

field :: Text -> [(Text,Value)] -> Either Text Value
field k = maybe (Left ("missing field: " <> k)) Right . lookupField k
textField :: Text -> [(Text,Value)] -> Either Text Text
textField k fs = field k fs >>= text
object :: Value -> Either Text [(Text,Value)]
object (Object fs) = Right fs
object _ = Left "expected object"
text :: Value -> Either Text Text
text (String t) = Right t
text _ = Left "expected text"
boolean :: Value -> Either Text Bool
boolean (Bool b) = Right b
boolean _ = Left "expected boolean"
list :: (Value -> Either Text a) -> Value -> Either Text [a]
list f (Array xs) = traverse f xs
list _ _ = Left "expected array"
optional :: (Value -> Either Text a) -> Value -> Either Text (Maybe a)
optional _ Null = Right Nothing
optional f v = Just <$> f v
index :: Value -> Either Text Int
index (Integer n) | n >= 0, n <= toInteger (maxBound :: Int) = Right (fromInteger n)
index _ = Left "expected nonnegative Int"
