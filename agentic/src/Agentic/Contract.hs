{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

-- | Contracts: two-way codecs with documentation. A contract says how to show a
-- value to a model, how to read one back, and what its schema looks like.
--
-- Generic deriving (@deriving (Generic, Contract)@ and
-- @deriving (Generic, Options)@) is GHC only: MicroHs's "GHC.Generics" has no
-- metadata classes to read names from. Under MicroHs, write contracts out
-- with 'record', 'Agentic.Contract.required', 'sumOf' and 'constructor', and
-- options with 'option'.
module Agentic.Contract
  ( -- * Codecs
    Codec (..)
  , Contract (..)
  , mapCodec
  , reschema
    -- * Records
  , ObjectCodec
  , record
  , required
  , requiredWith
  , optional
  , lmapObject
    -- * Sums
  , Case
  , sumOf
  , constructor
    -- * Adjusting contracts
  , documented
  , field
  , checked
  , between
#ifndef __MHS__
    -- * Generic deriving
  , genericContract
  , GContract (..)
  , GCases (..)
  , GCase (..)
  , GFields (..)
#endif
    -- * Enumerations
  , Options (..)
  , OptionSet (..)
  , Option (..)
  , option
  , documentedOptions
  , Enumeration (..)
  , enumeration
#ifndef __MHS__
  , GEnum (..)
#endif
  ) where

import Agentic.Schema
import Agentic.Value
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
#ifndef __MHS__
import Data.Kind (Type)
import GHC.Generics
#endif

-- ---------------------------------------------------------------------------
-- Codecs

data Codec a = Codec
  { schema :: Schema
  , encode :: a -> Value
  , decode :: Value -> Either Text a
  }

class Contract a where
  contract :: Codec a
#ifndef __MHS__
  default contract :: (Generic a, GContract (Rep a)) => Codec a
  contract = genericContract
#endif

mapCodec :: (a -> b) -> (b -> a) -> Codec a -> Codec b
mapCodec to' from' c = Codec c.schema (c.encode . from') (fmap to' . c.decode)

-- | Change a codec's schema. (A record update can't do this: t'Field' has a
-- @schema@ too, so @c {schema = ...}@ is ambiguous.)
reschema :: (Schema -> Schema) -> Codec a -> Codec a
reschema f (Codec s e d) = Codec (f s) e d

primitive :: Shape -> (a -> Value) -> (Value -> Either Text a) -> Codec a
primitive s = Codec (schemaOf s)

mismatch :: Text -> Value -> Either Text a
mismatch expected v = Left ("expected " <> expected <> ", got " <> renderJson v)

instance Contract Text where
  contract = primitive (SString Nothing) String $ \case
    String s -> Right s
    v -> mismatch "text" v

instance Contract Bool where
  contract = primitive SBool Bool $ \case
    Bool b -> Right b
    v -> mismatch "a boolean" v

instance Contract Integer where
  contract = primitive SInteger Integer $ \case
    Integer n -> Right n
    Number d | d == fromInteger (round d) -> Right (round d)
    v -> mismatch "an integer" v

instance Contract Int where
  contract = mapCodec fromInteger toInteger contract

instance Contract Double where
  contract = primitive SNumber Number $ \case
    Number d -> Right d
    Integer n -> Right (fromInteger n)
    v -> mismatch "a number" v

instance Contract () where
  contract = primitive SNull (const Null) (const (Right ()))

instance Contract a => Contract [a] where
  contract =
    let c = contract @a
     in Codec
          (schemaOf (SArray c.schema))
          (Array . map c.encode)
          ( \case
              Array vs -> traverse c.decode vs
              v -> mismatch "a list" v
          )

instance Contract a => Contract (Maybe a) where
  contract =
    let c = contract @a
     in Codec
          (schemaOf (SNullable c.schema))
          (maybe Null c.encode)
          ( \case
              Null -> Right Nothing
              v -> Just <$> c.decode v
          )

instance (Contract a, Contract b) => Contract (a, b) where
  contract =
    record "A pair" $
      (,) <$> required "_1" "" fst <*> required "_2" "" snd

instance (Contract a, Contract b, Contract c) => Contract (a, b, c) where
  contract =
    record "A triple" $
      (,,)
        <$> required "_1" "" (\(a, _, _) -> a)
        <*> required "_2" "" (\(_, b, _) -> b)
        <*> required "_3" "" (\(_, _, c) -> c)

-- ---------------------------------------------------------------------------
-- Records

-- | The fields of an object: encodes an @i@, decodes an @o@. Build one
-- applicatively with 'Agentic.Contract.required', then close it with 'record'.
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

lmapObject :: (j -> i) -> ObjectCodec i o -> ObjectCodec j o
lmapObject g (ObjectCodec fs e d) = ObjectCodec fs (e . g) d

-- | A described field, using the field type's contract.
required :: Contract a => Text -> Text -> (r -> a) -> ObjectCodec r a
required name d = requiredWith name (nonEmpty d) contract

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

-- | A field that may be absent.
optional :: Contract a => Text -> Text -> (r -> Maybe a) -> ObjectCodec r (Maybe a)
optional = required

-- | Close an object codec into a contract for a record.
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
data Case a = Case
  { tag :: Text
  , doc :: Maybe Text
  , fields :: [Field]
  , encode :: a -> Maybe [(Text, Value)]
  , decode :: [(Text, Value)] -> Either Text a
  }

-- | A constructor: its tag, a description, how to recognise it, and its fields.
--
-- > constructor "OneLiner" "A single line" isOneLiner (OneLiner <$> required "line" "" line)
constructor :: Text -> Text -> (a -> Bool) -> ObjectCodec a a -> Case a
constructor tag d matches o =
  Case
    tag
    (nonEmpty d)
    o.fields
    (\a -> if matches a then Just (o.encode a) else Nothing)
    o.decode

-- | A sum type. If no constructor has fields, it's encoded as an enumeration of
-- tags; otherwise each value is an object with a @tag@ field.
sumOf :: Text -> [Case a] -> Codec a
sumOf d = sumCodec (nonEmpty d)

sumCodec :: Maybe Text -> [Case a] -> Codec a
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

-- ---------------------------------------------------------------------------
-- Adjusting contracts

-- | Describe the whole type.
documented :: Text -> Codec a -> Codec a
documented d = reschema (documentedSchema d)

-- | Describe one field of a record (or of any constructor of a sum). Naming a
-- field that doesn't exist is an error when the schema is first used.
field :: Text -> Text -> Codec a -> Codec a
field name d = reschema (\s -> s {shape = update s.shape})
  where
    update = \case
      SObject fs | any named fs -> SObject (map describeField fs)
      SSum vs | any (any named . (.fields)) vs ->
        SSum [Variant t vd (map describeField vfs) | Variant t vd vfs <- vs]
      _ -> error ("Agentic.Contract.field: no field named " <> T.unpack name)
    named f = f.name == name
    describeField f@(Field n s r)
      | named f = Field n (documentedSchema d s) r
      | otherwise = f

-- | A constraint the wire schemas can't express. It's stated to the model and
-- checked locally; a value that fails it goes back to the model.
checked :: Text -> (a -> Bool) -> Codec a -> Codec a
checked rule ok c =
  Codec
    (s {checks = s.checks <> [rule]})
    c.encode
    ( \v -> do
        a <- c.decode v
        if ok a then Right a else Left ("must be " <> rule)
    )
  where
    s = c.schema

between :: (Ord a, Show a) => a -> a -> Codec a -> Codec a
between lo hi =
  checked
    ("between " <> T.pack (show lo) <> " and " <> T.pack (show hi))
    (\a -> a >= lo && a <= hi)

documentedSchema' :: Maybe Text -> Schema -> Schema
documentedSchema' = maybe id documentedSchema

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

#ifndef __MHS__
-- ---------------------------------------------------------------------------
-- Generic deriving

-- | A contract built from the type's 'Generic' representation, without
-- descriptions. Records become objects, sums become tagged objects, sums of
-- constructors without fields become enumerations, and a constructor with a
-- single unnamed field is transparent.
genericContract :: forall a. (Generic a, GContract (Rep a)) => Codec a
genericContract = mapCodec to from (gcontract @(Rep a))

class GContract (f :: Type -> Type) where
  gcontract :: Codec (f p)

instance (Datatype d, GCases f) => GContract (M1 D d f) where
  gcontract = named $ mapCodec M1 unM1 $ case gcases @f of
    [GCase _ (Just bare)] -> bare
    [GCase c Nothing] | not (null c.fields) -> recordFromCase c
    cs -> sumCodec Nothing (map (.gcase) cs)
    where
      named = reschema (titled (T.pack (datatypeName (undefined :: M1 D d f ()))))

recordFromCase :: Case a -> Codec a
recordFromCase c =
  Codec
    (schemaOf (SObject c.fields))
    (Object . fromMaybe [] . c.encode)
    ( \case
        Object kvs -> c.decode kvs
        v -> mismatch "an object" v
    )

data GCase a = GCase {gcase :: Case a, _gcaseBare :: Maybe (Codec a)}

class GCases (f :: Type -> Type) where
  gcases :: [GCase (f p)]

instance GCases V1 where
  gcases = []

instance (GCases f, GCases g) => GCases (f :+: g) where
  gcases = map (inject L1 (\case L1 x -> Just x; R1 _ -> Nothing)) (gcases @f)
    <> map (inject R1 (\case R1 x -> Just x; L1 _ -> Nothing)) (gcases @g)
    where
      inject :: (x -> y) -> (y -> Maybe x) -> GCase x -> GCase y
      inject wrap unwrap (GCase (Case t d fs e dec) _) =
        GCase (Case t d fs (\y -> unwrap y >>= e) (fmap wrap . dec)) Nothing

instance (Constructor c, GFields f) => GCases (M1 C c f) where
  gcases =
    [ GCase
        (Case tag Nothing o.fields (Just . o.encode) o.decode)
        (mapCodec M1 unM1 <$> gbare @f)
    ]
    where
      tag = T.pack (conName (undefined :: M1 C c f ()))
      o = lmapObject unM1 (M1 <$> snd (gfields @f 1))

class GFields (f :: Type -> Type) where
  -- | The fields, numbering unnamed ones from the given index.
  gfields :: Int -> (Int, ObjectCodec (f p) (f p))
  -- | The codec of a lone unnamed field, if that's what this is.
  gbare :: Maybe (Codec (f p))

instance GFields U1 where
  gfields n = (n, pure U1)
  gbare = Nothing

instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields n =
    let (n1, a) = gfields @f n
        (n2, b) = gfields @g n1
     in (n2, (:*:) <$> lmapObject (\(x :*: _) -> x) a <*> lmapObject (\(_ :*: y) -> y) b)
  gbare = Nothing

instance (Selector s, Contract a) => GFields (M1 S s (K1 i a)) where
  gfields n = (n + 1, M1 . K1 <$> requiredWith name Nothing contract (unK1 . unM1))
    where
      selector = selName (undefined :: M1 S s (K1 i a) ())
      name = if null selector then "_" <> T.pack (show n) else T.pack selector
  gbare
    | null (selName (undefined :: M1 S s (K1 i a) ())) = Just (mapCodec (M1 . K1) (unK1 . unM1) contract)
    | otherwise = Nothing

#endif

-- ---------------------------------------------------------------------------
-- Enumerations

data Option a = Option
  { value :: a
  , label :: Text
  , doc :: Maybe Text
  }

data OptionSet a = OptionSet
  { doc :: Maybe Text
  , options :: [Option a]
    -- ^ In order. For a score, this is the level order, lowest first.
  }

-- | Types whose values are a fixed, ordered list of described options. Jev's
-- @choice@ and @score@ need one.
class Options a where
  options :: OptionSet a
#ifndef __MHS__
  default options :: (Generic a, GEnum (Rep a), Show a) => OptionSet a
  options = OptionSet Nothing [Option v (showLabel v) Nothing | v <- map to (genum @(Rep a))]
#endif

-- | One option, labelled by 'show' (the constructor's name, for an enumeration).
option :: Show a => a -> Text -> Option a
option v d = Option v (showLabel v) (nonEmpty d)

-- | Options with a description of the whole set.
documentedOptions :: Text -> [Option a] -> OptionSet a
documentedOptions d = OptionSet (nonEmpty d)

-- | An option's label: what the model sees and answers with. For an
-- enumeration, 'show' gives the constructor's name.
showLabel :: Show a => a -> Text
showLabel = T.pack . show

-- | Use with @deriving via@ to give an 'Options' type a matching 'Contract':
--
-- > deriving via Enumeration Groan instance Contract Groan
newtype Enumeration a = Enumeration a

instance (Options a, Eq a) => Contract (Enumeration a) where
  contract = mapCodec Enumeration (\(Enumeration a) -> a) enumeration

-- | The contract of an 'Options' type: its labels, with their descriptions.
enumeration :: forall a. (Options a, Eq a) => Codec a
enumeration =
  Codec
    (documentedSchema' set.doc (schemaOf (SEnum [(o.label, o.doc) | o <- opts])))
    (\a -> maybe Null (String . (.label)) (find ((== a) . (.value)) opts))
    ( \case
        String t | Just o <- find ((== t) . (.label)) opts -> Right o.value
        v -> mismatch ("one of " <> T.intercalate ", " (map (.label) opts)) v
    )
  where
    set = options @a
    opts = set.options

#ifndef __MHS__
class GEnum (f :: Type -> Type) where
  genum :: [f p]

instance GEnum f => GEnum (M1 D d f) where
  genum = map M1 genum

instance (GEnum f, GEnum g) => GEnum (f :+: g) where
  genum = map L1 genum <> map R1 genum

instance GEnum (M1 C c U1) where
  genum = [M1 U1]
#endif
