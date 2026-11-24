{-# LANGUAGE TypeOperators #-}

-- | Full-value persistence, independent of model-facing contracts.
-- Derive Persist alongside Generic. Every Haskell field is saved, even when
-- its Contract deliberately omits it or maps it to a model-specific view.
--
-- Names are verbatim. Records and newtypes carry a constructor tag and a fields
-- object. All-nullary enums use strings; adding the first payload constructor
-- changes that wire form. Missing and extra fields fail decoding, without
-- defaults. Bump the flow revision when changing types or codecs.
--
-- Nested optional values round-trip. No Double, Float, Char or String instances
-- are supplied: JSON cannot preserve all their values faithfully in their usual
-- wire form. Use Text for strings or provide a custom codec. Recursive Generic schemas
-- raise RecursivePersistence when a codec or inferred flow step is forced,
-- including through describe, preflight or first interpretation. The guard
-- cannot see through custom instances or detect polymorphic recursion whose
-- type grows at each level; these schemas are unsupported.
module Agentic.Persist
  ( Persist (..), Persistence, persistence, persistWith, PersistenceError (..)
  , GPersist, PFields
  ) where

import Control.Exception (Exception, throw)
import Type.Reflection (Typeable, SomeTypeRep (..), typeRep)
import Agentic.Codec
import Agentic.Schema
import Agentic.Value
import Data.Kind (Type)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics

-- | A codec builder. Custom instances use persistWith; generic derivation
-- threads an ancestor path so recursive schemas fail with a named error.
newtype Persistence a = Persistence ([SomeTypeRep] -> Codec a)

data PersistenceError = RecursivePersistence Text deriving (Eq, Show)
instance Exception PersistenceError

class Typeable a => Persist a where
  persist :: Persistence a
  default persist :: (Generic a, GPersist (Rep a)) => Persistence a
  persist = Persistence $ \ancestors ->
    let rep = SomeTypeRep (typeRep @a)
    in if rep `elem` ancestors
       then throw (RecursivePersistence (T.pack (show rep)))
       else mapCodec to from (sumOf "" (pcases @(Rep a) (rep:ancestors)))

-- | Wrap a user-supplied codec. The user owns its round-trip and finite-schema
-- laws. A hand-written instance can hide recursion from the generic guard.
persistWith :: Codec a -> Persistence a
persistWith c = Persistence (const c)

-- | Build and check the complete schema before exposing the codec. Recursive
-- Generic types throw RecursivePersistence here, rather than hanging during
-- journal writes. Reusing a type in separate branches is fine.
persistence :: forall a. Persist a => Codec a
persistence = let c = build @a [] in forceSchema c.schema `seq` c

build :: forall a. Persist a => [SomeTypeRep] -> Codec a
build path = case persist @a of Persistence f -> f path

forceSchema :: Schema -> ()
forceSchema s = case s.shape of
  SObject fs -> foldr (\f rest -> forceSchema f.schema `seq` rest) () fs
  SSum cs -> foldr (\c rest -> foldr (\f r -> forceSchema f.schema `seq` r) rest c.fields) () cs
  SArray inner -> forceSchema inner
  SNullable inner -> forceSchema inner
  _ -> ()

-- | Internal constraint, exported only for the Generic default signature.
class GPersist (f :: Type -> Type) where
  pcases :: [SomeTypeRep] -> [CaseCodec (f p)]
instance GPersist f => GPersist (M1 D d f) where
  pcases path = map (wrapCase M1 (Just . unM1)) (pcases @f path)
instance (GPersist f, GPersist g) => GPersist (f :+: g) where
  pcases path = map (wrapCase L1 (\case L1 a -> Just a; _ -> Nothing)) (pcases @f path)
    <> map (wrapCase R1 (\case R1 a -> Just a; _ -> Nothing)) (pcases @g path)
instance (Constructor c, PFields f) => GPersist (M1 C c f) where
  pcases path = [CaseCodec tag Nothing fields encode decode]
   where
    tag = T.pack (conName (undefined :: M1 C c f ()))
    obj = snd (pfields @f path 1)
    fields = if null obj.fields then [] else [Field "fields" (schemaOf (SObject obj.fields)) True]
    encode x = Just (if null obj.fields then [] else [("fields",Object (obj.encode (unM1 x)))])
    decode fs = do
      exactFields (if null fs then [] else "tag" : map (.name) fields) fs
      payload <- if null fields then Right [] else case lookupField "fields" fs of
        Just (Object xs) -> Right xs
        _ -> Left "expected constructor fields"
      exactFields (map (.name) obj.fields) payload
      M1 <$> obj.decode payload

wrapCase :: (a -> b) -> (b -> Maybe a) -> CaseCodec a -> CaseCodec b
wrapCase f g c = CaseCodec c.tag c.doc c.fields (\b -> g b >>= c.encode) (fmap f . c.decode)

-- | Internal field constraint, exported only for the Generic default signature.
class PFields (f :: Type -> Type) where
  pfields :: [SomeTypeRep] -> Int -> (Int, ObjectCodec (f p) (f p))
instance PFields U1 where pfields _ n = (n,pure U1)
instance (PFields f, PFields g) => PFields (f :*: g) where
  pfields path n =
    let (m,a) = pfields @f path n; (k,b) = pfields @g path m
    in (k, (:*:) <$> lmapObject (\(x :*: _) -> x) a <*> lmapObject (\(_ :*: y) -> y) b)
instance (Selector s, Persist a) => PFields (M1 S s (K1 i a)) where
  pfields path n = (n+1,M1 . K1 <$> ObjectCodec [Field name codec.schema True]
    (\x -> [(name,codec.encode (unK1 (unM1 x)))])
    (\fs -> maybe (Left ("missing field " <> name)) codec.decode (lookupField name fs)))
   where
    codec = build @a path
    selector = selName (undefined :: M1 S s (K1 i a) ())
    name = if null selector then "_" <> T.pack (show n) else T.pack selector

exactFields :: [Text] -> [(Text,Value)] -> Either Text ()
exactFields expected fields
  | sort expected == sort (map fst fields) = Right ()
  | otherwise = Left ("stored fields differ: expected " <> T.intercalate ", " expected)

instance Persist Text where
  persist = persistWith $ primitive (SString Nothing) String $ \case
    String t -> Right t
    v -> mismatch "text" v
instance Persist Bool where
  persist = persistWith $ primitive SBool Bool $ \case
    Bool b -> Right b
    v -> mismatch "a boolean" v
instance Persist Integer where
  persist = persistWith $ primitive SInteger Integer $ \case
    Integer n -> Right n
    v -> mismatch "an integer" v
instance Persist Int where
  persist = persistWith $ primitive SInteger (Integer . toInteger) $ \case
    Integer n | n >= toInteger (minBound :: Int), n <= toInteger (maxBound :: Int) ->
      Right (fromInteger n)
    v -> mismatch "an Int in range" v
instance Persist () where
  persist = persistWith $ primitive SNull (const Null) $ \case
    Null -> Right ()
    v -> mismatch "null" v
instance Persist a => Persist [a] where
  persist = Persistence $ \path ->
    let codec = build @a path
    in primitive (SArray codec.schema) (Array . map codec.encode) $ \case
      Array xs -> traverse codec.decode xs
      v -> mismatch "an array" v
-- Generic tagged constructors distinguish Nothing, Just Nothing and Just ().
instance Persist a => Persist (Maybe a)
instance (Persist a, Persist b) => Persist (Either a b)
instance (Persist a, Persist b) => Persist (a,b)
instance (Persist a, Persist b, Persist c) => Persist (a,b,c)
