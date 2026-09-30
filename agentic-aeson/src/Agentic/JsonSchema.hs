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
  , schemaName
  , wrap
  , unwrap
  ) where

import Agentic.Schema
import Agentic.Value
import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

-- | A strict JSON Schema: every object lists all its fields as required and
-- forbids others; nullable fields may be null. Types are written out in full
-- wherever they appear; 'objectSchema' shares repeated ones.
jsonSchema :: Schema -> Value
jsonSchema = schemaWith []

-- | Like 'jsonSchema', but any schema named in @shared@ becomes a @$ref@ into
-- @$defs@.
schemaWith :: [Text] -> Schema -> Value
schemaWith shared s = case title s of
  Just t | t `elem` shared -> Object [("$ref", String ("#/$defs/" <> t))]
  _ -> definition shared s

-- | A schema written out, though the schemas inside it may still be references.
definition :: [Text] -> Schema -> Value
definition shared s = withDescription (body (shape s))
  where
    sub = schemaWith shared
    withDescription = \case
      Object kvs | Just d <- description -> Object (kvs <> [("description", String d)])
      v -> v
    description = case catMaybes [doc s] <> map (\c -> "Must be " <> c <> ".") (checks s) of
      [] -> Nothing
      ds -> Just (T.intercalate " " ds)
    body = \case
      SObject fs -> object (map (\f -> (fieldName f, sub (fieldSchema f))) fs)
      SSum vs -> Object [("anyOf", Array (map variant vs))]
      SEnum ls
        | all ((== Nothing) . snd) ls -> Object [typed "string", ("enum", Array (map (String . fst) ls))]
        | otherwise -> Object [("anyOf", Array [constant l d | (l, d) <- ls])]
      SArray inner -> Object [typed "array", ("items", sub inner)]
      SNullable inner -> Object [("anyOf", Array [sub inner, Object [typed "null"]])]
      SString format -> Object (typed "string" : maybe [] (\f -> [("format", String (formatName f))]) format)
      SInteger -> Object [typed "integer"]
      SNumber -> Object [typed "number"]
      SBool -> Object [typed "boolean"]
      SNull -> Object [typed "null"]
    variant v =
      let tagged = ("tag", Object [typed "string", ("const", String (variantTag v))])
          o = object (tagged : map (\f -> (fieldName f, sub (fieldSchema f))) (variantFields v))
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
-- object with a single @value@ field holding it. A named type that appears more
-- than once, identically, is written once under @$defs@ and referenced.
objectSchema :: Schema -> Value
objectSchema s = case root of
  Object kvs | not (null defs) -> Object (kvs <> [("$defs", Object defs)])
  other -> other
  where
    root
      | wrap s = object [("value", schemaWith shared s)]
      | otherwise = definition shared s
    shared = sharedNames s
    defs = [(t, definition shared d) | t <- shared, Just d <- [lookup t named']]
    named' = [(t, d) | d <- nested s, Just t <- [title d]]

-- | Names of the types inside a schema (not the schema itself) that appear more
-- than once and are identical everywhere they appear, in order of appearance.
sharedNames :: Schema -> [Text]
sharedNames s = [t | t <- nub names, uses t >= 2, length (nub [d | d <- inside, title d == Just t]) == 1]
  where
    inside = nested s
    names = catMaybes (map title inside)
    uses t = length (filter ((== Just t) . title) inside)

-- | Every schema nested inside this one, outermost first.
nested :: Schema -> [Schema]
nested s = concatMap (\c -> c : nested c) (children (shape s))
  where
    children = \case
      SObject fs -> map fieldSchema fs
      SSum vs -> concatMap (map fieldSchema . variantFields) vs
      SArray c -> [c]
      SNullable c -> [c]
      _ -> []

-- | A name for the schema, for providers that ask for one: the type's name with
-- anything but letters, digits, @_@ and @-@ dropped, or @output@.
schemaName :: Schema -> Text
schemaName s = case T.intercalate "_" (filter (not . T.null) (T.split (not . valid) (maybe "" id (title s)))) of
  "" -> "output"
  n -> T.take 64 n
  where
    valid c = isAlphaNum c || c == '_' || c == '-'

-- | Undo 'objectSchema''s wrapping on a value from the provider.
unwrap :: Schema -> Value -> Value
unwrap s v
  | wrap s, Object kvs <- v, Just inner <- lookup "value" kvs = inner
  | otherwise = v
