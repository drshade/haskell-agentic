-- | Wire values for the journal, independent of provider JSON libraries.
module Agentic.Internal.Replay where

import Agentic.Runtime
import Agentic.Core (Instruction (..), Note (..))
import Agentic.Questions
import Agentic.Schema
import Agentic.Value
import Data.Text (Text)

encodeStoredTurn :: Turn -> Value
encodeStoredTurn (Turn (Raw r) a) = Object [("raw", r), ("action", act a)]
  where
    act = \case
      CallTools calls -> Object [("callTools", Array [Object [("id", String c.callId), ("name", String c.name), ("input", c.input)] | c <- calls])]
      Respond v -> Object [("respond", v)]

decodeStoredTurn :: Value -> Maybe Turn
decodeStoredTurn = \case
  Object kvs -> do
    r <- lookupField "raw" kvs
    a <- lookupField "action" kvs
    Turn (Raw r) <$> act a
  _ -> Nothing
  where
    act = \case
      Object [("respond", v)] -> Just (Respond v)
      Object [("callTools", Array calls)] -> CallTools <$> traverse call calls
      _ -> Nothing
    call = \case
      Object kvs -> ToolCall <$> text "id" kvs <*> text "name" kvs <*> lookupField "input" kvs
      _ -> Nothing
    text k kvs = case lookupField k kvs of
      Just (String t) -> Just t
      _ -> Nothing

encodeStoredAnswers :: [Answer] -> Value
encodeStoredAnswers = Array . map answer
  where
    answer = \case
      YesNoAnswer p -> Object [("yesNo", prob p)]
      ChoiceAnswer l ps c -> Object [("choice", String l), ("probabilities", Array [Array [String x, prob p] | (x, p) <- ps]), ("confidence", prob c)]
      ScoreAnswer pos ps c -> Object [("score", Number pos), ("probabilities", Array [Array [Integer (toInteger i), prob p] | (i, p) <- ps]), ("confidence", prob c)]
    prob = Integer . toInteger . basisPoints

decodeStoredAnswers :: Value -> Maybe [Answer]
decodeStoredAnswers = \case
  Array xs -> traverse answer xs
  _ -> Nothing
  where
    -- Look fields up by name: the file comes back through aeson, which
    -- reorders keys.
    answer = \case
      Object kvs
        | Just p <- lookupField "yesNo" kvs -> YesNoAnswer <$> prob p
        | Just (String l) <- lookupField "choice" kvs ->
            ChoiceAnswer l <$> (pairs label =<< lookupField "probabilities" kvs) <*> (prob =<< lookupField "confidence" kvs)
        | Just pos <- lookupField "score" kvs ->
            ScoreAnswer <$> number pos <*> (pairs index =<< lookupField "probabilities" kvs) <*> (prob =<< lookupField "confidence" kvs)
      _ -> Nothing
    pairs key = \case
      Array ps -> traverse (\case Array [k, p] -> (,) <$> key k <*> prob p; _ -> Nothing) ps
      _ -> Nothing
    label = \case
      String t -> Just t
      _ -> Nothing
    index = \case
      Integer i | i >= toInteger (minBound :: Int) && i <= toInteger (maxBound :: Int) -> Just (fromInteger i)
      _ -> Nothing
    prob = \case
      Integer bp | bp >= 0 && bp <= 10000 -> Just (fromBasisPoints (fromInteger bp))
      _ -> Nothing
    number = \case
      Number d -> Just d
      Integer n -> Just (fromInteger n)
      _ -> Nothing

schemaValue :: Schema -> Value
schemaValue s = Object
  [("title",maybe Null String s.title),("doc",maybe Null String s.doc)
  ,("checks",Array (map String s.checks)),("shape",shapeValue s.shape)]
 where
  node k xs = Object (("kind",String k):xs)
  fields fs = Array [Object [("name",String f.name),("required",Bool f.required)
                           ,("schema",schemaValue f.schema)] | f <- fs]
  shapeValue = \case
    SObject fs -> node "object" [("fields",fields fs)]
    SSum vs -> node "sum" [("variants",Array [Object
      [("tag",String v.tag),("doc",maybe Null String v.doc),("fields",fields v.fields)] | v <- vs])]
    SEnum opts -> node "enum" [("options",Array [Array [String k,maybe Null String d] | (k,d) <- opts])]
    SArray a -> node "array" [("item",schemaValue a)]
    SNullable a -> node "nullable" [("item",schemaValue a)]
    SString f -> node "string" [("format",maybe Null (String . format) f)]
    SInteger -> node "integer" []; SNumber -> node "number" []
    SBool -> node "bool" []; SNull -> node "null" []
  format = \case
    DateTime -> "dateTime"; Date -> "date"; Email -> "email"; Uri -> "uri"; Uuid -> "uuid"

judgeRequestValue :: JudgeRequest -> Value
judgeRequestValue r = Object [("input",r.input),("questions",Array (map spec r.questions))]
 where
  spec = \case
    AskYesNo q -> Object [("yesNo",String q)]
    AskChoice q os -> Object [("choice",String q),("options",labels os)]
    AskScore q os -> Object [("score",String q),("options",labels os)]
  labels xs = Array [Array [String k,maybe Null String d] | (k,d) <- xs]

conversationValue :: Conversation -> Value
conversationValue c = Object
  [("instruction",String c.instruction.text),("input",c.input)
  ,("inputSchema",schemaValue c.inputSchema),("outputSchema",schemaValue c.outputSchema)
  ,("path",Array [Object [("name",String n.name),("description",maybe Null String n.description)] | n <- c.path])
  ,("tools",Array [Object [("name",String t.name),("description",String t.description)
                         ,("input",schemaValue t.input)] | t <- c.tools])
  ,("history",Array (map exchange c.history))]
 where
  exchange = \case
    Called (Raw r) rs -> Object [("called",r),("results",Array [Array [String i,toolResultValue x] | (i,x) <- rs])]
    Rejected (Raw r) p -> Object [("rejected",r),("problem",String p)]

toolResultValue :: ToolResult -> Value
toolResultValue = \case
  ToolOk v -> Object [("ok",v)]
  ToolFailed t -> Object [("failed",String t)]

decodeToolResult :: Value -> Either Text ToolResult
decodeToolResult = \case
  Object [("ok",v)] -> Right (ToolOk v)
  Object [("failed",String t)] -> Right (ToolFailed t)
  _ -> Left "invalid tool result"

-- | Completion envelopes retain outcomes separately from provider-facing text.
completionValue :: (Outcome, ToolResult) -> Value
completionValue (outcome,result) = Object
  [("outcome",String (case outcome of
      CallOk -> "ok"; UnknownTool -> "unknownTool"; InvalidInput -> "invalidInput"; Completed -> "completed"))
  ,("result",toolResultValue result)]

decodeCompletion :: Value -> Either Text (Outcome, ToolResult)
decodeCompletion (Object fs) = do
  outcome <- case lookupField "outcome" fs of
    Just (String "ok") -> Right CallOk
    Just (String "unknownTool") -> Right UnknownTool
    Just (String "invalidInput") -> Right InvalidInput
    _ -> Left "invalid tool completion outcome"
  result <- maybe (Left "missing tool result") decodeToolResult (lookupField "result" fs)
  pure (outcome,result)
decodeCompletion _ = Left "invalid tool completion"

fromMaybeDecode :: Text -> (Value -> Maybe a) -> Value -> Either Text a
fromMaybeDecode what f = maybe (Left what) Right . f
