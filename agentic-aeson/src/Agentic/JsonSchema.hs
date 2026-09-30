-- | Lowering the core's 'Schema' to the strict JSON Schema that providers'
-- structured outputs and strict tools accept.
--
-- The result is the core's 'Value', not aeson's, because order matters: a
-- model writes an object's fields in the order its schema lists them, and a
-- record's field order is part of its meaning (a setup comes before its
-- punchline). aeson would sort the keys. Render with 'renderJson'.
module Agentic.JsonSchema
  ( jsonSchema
  , objectSchema
  , wrap
  , unwrap
  ) where

import Agentic.Schema
import Agentic.Value
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

-- | A strict JSON Schema: every object lists all its fields as required and
-- forbids others; nullable fields may be null.
jsonSchema :: Schema -> Value
jsonSchema s = withDescription (body (shape s))
  where
    withDescription = \case
      Object kvs | Just d <- description -> Object (kvs <> [("description", String d)])
      v -> v
    description = case catMaybes [doc s] <> map (\c -> "Must be " <> c <> ".") (checks s) of
      [] -> Nothing
      ds -> Just (T.intercalate " " ds)
    body = \case
      SObject fs -> object (map (\f -> (fieldName f, jsonSchema (fieldSchema f))) fs)
      SSum vs -> Object [("anyOf", Array (map variant vs))]
      SEnum ls
        | all ((== Nothing) . snd) ls -> Object [typed "string", ("enum", Array (map (String . fst) ls))]
        | otherwise -> Object [("anyOf", Array [constant l d | (l, d) <- ls])]
      SArray inner -> Object [typed "array", ("items", jsonSchema inner)]
      SNullable inner -> Object [("anyOf", Array [jsonSchema inner, Object [typed "null"]])]
      SString format -> Object (typed "string" : maybe [] (\f -> [("format", String (formatName f))]) format)
      SInteger -> Object [typed "integer"]
      SNumber -> Object [typed "number"]
      SBool -> Object [typed "boolean"]
      SNull -> Object [typed "null"]
    variant v =
      let tagged = ("tag", Object [typed "string", ("const", String (variantTag v))])
          o = object (tagged : map (\f -> (fieldName f, jsonSchema (fieldSchema f))) (variantFields v))
       in case (o, variantDoc v) of
            (Object kvs, Just d) -> Object (kvs <> [("description", String d)])
            _ -> o
    constant l d = Object ([typed "string", ("const", String l)] <> maybe [] (\t -> [("description", String t)]) d)
    typed :: Text -> (Text, Value)
    typed t = ("type", String t)

object :: [(Text, Value)] -> Value
object fields =
  Object
    [ ("type", String "object")
    , ("properties", Object fields)
    , ("required", Array (map (String . fst) fields))
    , ("additionalProperties", Bool False)
    ]

formatName :: Format -> Text
formatName = \case
  DateTime -> "date-time"
  Date -> "date"
  Email -> "email"
  Uri -> "uri"
  Uuid -> "uuid"

-- | Does this schema need wrapping to be a top-level object?
wrap :: Schema -> Bool
wrap s = case shape s of
  SObject _ -> False
  _ -> True

-- | A top-level object schema: the schema itself if it's an object, or an
-- object with a single @value@ field holding it.
objectSchema :: Schema -> Value
objectSchema s
  | wrap s = object [("value", jsonSchema s)]
  | otherwise = jsonSchema s

-- | Undo 'objectSchema''s wrapping on a value from the provider.
unwrap :: Schema -> Value -> Value
unwrap s v
  | wrap s, Object kvs <- v, Just inner <- lookup "value" kvs = inner
  | otherwise = v
