-- | Compiled codecs and builders for projection interpreters. Structural contracts live in
-- "Agentic.Contract"; this module contains no contract instances.
module Agentic.Codec
  ( Codec (..)
  , mapCodec
  , reschema
  , primitive
  , mismatch
  , ObjectCodec (..)
  , record
  , requiredWith
  , lmapObject
  , CaseCodec (..)
  , sumOf
  ) where

import Agentic.Schema
import Agentic.Value
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

-- | A compiled schema with a matching typed encoder and validating decoder.
data Codec a = Codec
  { schema :: Schema
  , encode :: a -> Value
  , decode :: Value -> Either Text a
  }

-- | Change the Haskell value type using inverse mappings, retaining the schema.
mapCodec :: (a -> b) -> (b -> a) -> Codec a -> Codec b
mapCodec to' from' c = Codec c.schema (c.encode . from') (fmap to' . c.decode)

-- | Change a codec's schema. (A record update can't do this: t'Field' has a
-- @schema@ too, so @c {schema = ...}@ is ambiguous.)
reschema :: (Schema -> Schema) -> Codec a -> Codec a
reschema f (Codec s e d) = Codec (f s) e d

-- | Build a leaf codec from its projected shape and typed conversion functions.
primitive :: Shape -> (a -> Value) -> (Value -> Either Text a) -> Codec a
primitive s = Codec (schemaOf s)

-- | Report an unexpected value with its expected shape and JSON rendering.
mismatch :: Text -> Value -> Either Text a
mismatch expected v = Left ("expected " <> expected <> ", got " <> renderJson v)

-- | Compiled fields: encode an input object and applicatively reconstruct an
-- output object. Field order is retained for schemas and encoded values.
data ObjectCodec i o = ObjectCodec
  { fields :: [Field]
  , encode :: i -> [(Text, Value)]
  , decode :: [(Text, Value)] -> Either Text o
  }

instance Functor (ObjectCodec i) where
  fmap f (ObjectCodec fs e d) = ObjectCodec fs e (fmap f . d)

instance Applicative (ObjectCodec i) where
  pure x = ObjectCodec [] (const []) (const (Right x))
  f <*> x =
    ObjectCodec
      (f.fields <> x.fields)
      (\i -> f.encode i <> x.encode i)
      (\kvs -> f.decode kvs <*> x.decode kvs)

-- | Adapt the input supplied to a compiled object's field getters.
lmapObject :: (j -> i) -> ObjectCodec i o -> ObjectCodec j o
lmapObject g (ObjectCodec fs e d) = ObjectCodec fs (e . g) d

-- | A field with an explicit codec. Nullable fields ('Maybe') may be absent.
requiredWith :: Text -> Maybe Text -> Codec a -> (r -> a) -> ObjectCodec r a
requiredWith name d c get =
  ObjectCodec
    [Field name schema (not nullable)]
    (\r -> [(name, c.encode (get r))])
    ( \kvs -> case lookupField name kvs of
        Just v -> prefix (c.decode v)
        Nothing
          | nullable -> prefix (c.decode Null)
          | otherwise -> Left ("missing field " <> name)
    )
 where
  schema = maybe id documentedSchema d c.schema
  nullable = case c.schema.shape of
    SNullable _ -> True
    _ -> False
  prefix = either (\e -> Left (name <> ": " <> e)) Right

-- | Close compiled fields into a documented record codec.
record :: Text -> ObjectCodec a a -> Codec a
record d o =
  Codec
    (documentedSchema' (nonEmpty d) (schemaOf (SObject o.fields)))
    (Object . o.encode)
    ( \case
        Object kvs -> o.decode kvs
        v -> mismatch "an object" v
    )

-- ---------------------------------------------------------------------------
-- Sums

-- | One constructor of a sum type.
data CaseCodec a = CaseCodec
  { tag :: Text
  , doc :: Maybe Text
  , fields :: [Field]
  , encode :: a -> Maybe [(Text, Value)]
  , decode :: [(Text, Value)] -> Either Text a
  }

-- | A sum type. If no constructor has fields, it's encoded as an enumeration of
-- tags; otherwise each value is an object with a @tag@ field.
sumOf :: Text -> [CaseCodec a] -> Codec a
sumOf d = sumCodec (nonEmpty d)

sumCodec :: Maybe Text -> [CaseCodec a] -> Codec a
sumCodec d cases
  | all (null . (.fields)) cases =
      Codec
        (documentedSchema' d (schemaOf (SEnum [(c.tag, c.doc) | c <- cases])))
        (\a -> maybe Null (String . (.tag)) (matching a))
        ( \case
            String t | Just c <- byTag t -> c.decode []
            v -> mismatch ("one of " <> T.intercalate ", " (map (.tag) cases)) v
        )
  | otherwise =
      Codec
        (documentedSchema' d (schemaOf (SSum [Variant c.tag c.doc c.fields | c <- cases])))
        ( \a -> case [(c.tag, kvs) | c <- cases, Just kvs <- [c.encode a]] of
            (t, kvs) : _ -> Object (("tag", String t) : kvs)
            [] -> Null
        )
        ( \case
            Object kvs
              | Just (String t) <- lookupField "tag" kvs ->
                  maybe (Left ("unknown tag " <> t)) (\c -> c.decode kvs) (byTag t)
            v -> mismatch "an object with a tag" v
        )
 where
  matching a = find (\c -> isJust (c.encode a)) cases
  byTag t = find ((== t) . (.tag)) cases

documentedSchema' :: Maybe Text -> Schema -> Schema
documentedSchema' = maybe id documentedSchema

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t
