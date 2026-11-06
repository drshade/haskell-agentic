{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | One typed structural contract, interpreted into independently chosen codecs.
module Agentic.Contract
  ( Contract (..)
  , Structure (..)
  , Fields (..)
  , Case (..)
  , Constructor (..)
  , FieldStyle (..)
  , ScalarInfo (..)
  , Outline (..)
  , OutlineField (..)
  , OutlineCase (..)
  , Codec (..)
  , ProjectionError (..)
  , compileModel
  , compileWith
  , compileFieldsWith
  , compileCaseWith
  , outline
  , outlineLabel
  , scalar
  , nominal
  , mapStructure
  , mapCodec
  , reschema
  , record
  , required
  , requiredWith
  , optional
  , lmapObject
  , sumOf
  , constructor
  , documented
  , field
  , checked
  , between
  , genericContract
  , GContract (..)
  , GCases (..)
  , GFields (..)
  , GEnum (..)
  , Options (..)
  , OptionSet (..)
  , Option (..)
  , option
  , documentedOptions
  , enumeration
  ) where

import Agentic.Codec (Codec (..), mapCodec, reschema)
import Agentic.Codec qualified as C
import Agentic.Schema hiding (Field)
import Agentic.Schema qualified as Schema
import Agentic.Value
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics hiding (Constructor)
import GHC.Generics qualified
import Type.Reflection

-- | Stable semantic identity chosen by the definer and explicit parameters.
-- The identity need not be a Haskell module path. Type evidence is
-- carried separately by 'Scalar'; do not identify scalars by an unqualified name.
data ScalarInfo = ScalarInfo {name :: Text, parameters :: [(Text, Text)]}
  deriving (Eq, Show)

-- | Whether the author declared named record fields or positional arguments.
data FieldStyle = RecordFields | PositionalFields deriving (Eq, Show)

-- | The declared constructor name and field style, independent of wire layout.
data Constructor = Constructor {name :: Text, style :: FieldStyle} deriving (Eq, Show)

-- | Executable structure. No projection is chosen by constructing this tree.
-- Mappings preserve the inner structure; use Named for a nominal wrapper and
-- Scalar for a deliberately opaque semantic leaf.
data Structure a where
  Scalar :: Typeable a => ScalarInfo -> Maybe (Codec a) -> Structure a
  Named :: TypeRep a -> Structure a -> Structure a
  Record :: Constructor -> Fields a a -> Structure a
  Sum :: [Case a] -> Structure a
  List :: Structure a -> Structure [a]
  Optional :: Structure a -> Structure (Maybe a)
  Unit :: Structure ()
  Mapped :: (a -> b) -> (b -> a) -> Structure a -> Structure b
  Checked :: Text -> (a -> Bool) -> Structure a -> Structure a
  Documented :: Text -> Structure a -> Structure a

-- | Applicative fields retain getters, reconstruction, and declaration order.
data Fields r a where
  Pure :: a -> Fields r a
  Apply :: Fields r (a -> b) -> Fields r a -> Fields r b
  Field :: Text -> Maybe Text -> Structure a -> (r -> a) -> Fields r a

instance Functor (Fields r) where
  fmap f x = Apply (Pure f) x
instance Applicative (Fields r) where
  pure = Pure
  (<*>) = Apply

-- | Adapt the input seen by field getters, retaining their order and reconstruction.
lmapObject :: (s -> r) -> Fields r a -> Fields s a
lmapObject f = \case
  Pure a -> Pure a
  Apply g x -> Apply (lmapObject f g) (lmapObject f x)
  Field n d c get -> Field n d c (get . f)

-- | A sum case exposes its payload only after successful matching. The payload
-- type is existential: generic field getters cannot be applied to another arm.
data Case a where
  Case ::
    { constructor :: Constructor
    , doc :: Maybe Text
    , match :: a -> Maybe p
    , inject :: p -> a
    , fields :: Fields p p
    } ->
    Case a

-- | A single structural authority for discovery and independently compiled codecs.
-- Derive with Generic, or build a tree explicitly with record, sumOf and scalar.
class Contract a where
  -- | The typed description; no storage or model projection is selected yet.
  contract :: Structure a
  default contract :: (Generic a, Typeable a, GContract (Rep a)) => Structure a
  contract = genericContract

-- | Define an identifiable semantic leaf with optional default model behavior.
-- Other interpreters can supply codecs even when that default is absent.
scalar :: Typeable a => ScalarInfo -> Maybe (Codec a) -> Structure a
scalar = Scalar

-- | Preserve a type's resolved nominal identity and applied arguments. Useful
-- around handwritten contracts and mappings; generic derivation adds it itself.
nominal :: forall a. Typeable a => Structure a -> Structure a
nominal = Named (typeRep @a)

-- | Change the Haskell representation without discarding the inner structure.
-- The functions should be inverse on supported values. Wrap with nominal
-- when the outer type has a nominal identity that discovery must preserve.
mapStructure :: (a -> b) -> (b -> a) -> Structure a -> Structure b
mapStructure = Mapped

-- | Close applicative fields into a documented record description. Use Named
-- (or nominal) when a handwritten record also needs nominal identity.
record :: Text -> Fields a a -> Structure a
record d = documented d . Record (Constructor "" RecordFields)

-- | A named, documented field using its type's contract and a getter.
required :: Contract a => Text -> Text -> (r -> a) -> Fields r a
required n d = requiredWith n (nonEmpty d) contract

-- | A named field with an explicit structural description. Missing-field
-- behavior is chosen by the projection; the model permits absent Maybe fields.
requiredWith :: Text -> Maybe Text -> Structure a -> (r -> a) -> Fields r a
requiredWith = Field

-- | A Maybe field. Models use null/absence; other projections may use tags.
optional :: Contract a => Text -> Text -> (r -> Maybe a) -> Fields r (Maybe a)
optional = required

-- | A handwritten sum case with a predicate and applicative fields. Getters
-- must be defined for every value accepted by the predicate. For typed payload
-- matching and injection, use the existential Case constructor directly.
constructor :: Text -> Text -> (a -> Bool) -> Fields a a -> Case a
constructor n d matches = Case (Constructor n RecordFields) (nonEmpty d) (\a -> if matches a then Just a else Nothing) id

-- | Describe an ordered set of sum cases. The interpreter chooses their layout.
sumOf :: Text -> [Case a] -> Structure a
sumOf d = documented d . Sum

-- | Describe the whole contract, without selecting a representation.
documented :: Text -> Structure a -> Structure a
documented "" = id
documented d = Documented d

-- | Describe one field of a record or each matching sum arm. Annotation is
-- applied immediately so it follows layout changes. A missing field raises an
-- error when the annotated structure is first evaluated.
field :: Text -> Text -> Structure a -> Structure a
field n d s = case annotateField n d s of
  Just annotated -> annotated
  Nothing -> error ("Agentic.Contract.field: no field named " <> T.unpack n)

-- | Share a semantic predicate across projections. Decoders reject values that
-- fail it; model schemas also carry the rule as explanatory text.
checked :: Text -> (a -> Bool) -> Structure a -> Structure a
checked = Checked

-- | Require a decoded value to lie within the inclusive bounds.
between :: (Ord a, Show a) => a -> a -> Structure a -> Structure a
between lo hi = checked ("between " <> T.pack (show lo) <> " and " <> T.pack (show hi)) (\a -> a >= lo && a <= hi)

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

-- | Pure discovery data derived from the structure, independent of any codec.
-- Representation-only mappings are omitted.
data Outline
  = OScalar SomeTypeRep ScalarInfo
  | ONamed SomeTypeRep Outline
  | ORecord Constructor [OutlineField]
  | OSum [OutlineCase]
  | OList Outline
  | OOptional Outline
  | OUnit
  | OChecked Text Outline
  | ODocumented Text Outline
  deriving (Eq, Show)

-- | A field's name, optional documentation, and structural outline.
data OutlineField = OutlineField
  { name :: Text
  , doc :: Maybe Text
  , structure :: Outline
  }
  deriving (Eq, Show)

-- | Constructor metadata, optional documentation, and fields in declaration order.
data OutlineCase = OutlineCase
  { constructor :: Constructor
  , doc :: Maybe Text
  , fields :: [OutlineField]
  }
  deriving (Eq, Show)

-- | Discover a finite contract without compiling or invoking any codec.
-- Functions and predicates remain in the tree rather than this pure view.
outline :: forall a. Structure a -> Outline
outline = \case
  Scalar info _ -> OScalar (SomeTypeRep (typeRep @a)) info
  Named rep c -> ONamed (SomeTypeRep rep) (outline c)
  Record con fs -> ORecord con (describeFields fs)
  Sum cs -> OSum [OutlineCase con d (describeFields fs) | Case con d _ _ fs <- cs]
  List c -> OList (outline c)
  Optional c -> OOptional (outline c)
  Unit -> OUnit
  Mapped _ _ c -> outline c
  Checked rule _ c -> OChecked rule (outline c)
  Documented d c -> ODocumented d (outline c)

describeFields :: Fields r a -> [OutlineField]
describeFields = \case
  Pure _ -> []
  Apply f x -> describeFields f <> describeFields x
  Field n d c _ -> [OutlineField n d (outline c)]

-- | A concise display label. Scalar identity strings remain available separately.
outlineLabel :: Outline -> Text
outlineLabel = \case
  ONamed rep _ -> T.pack (show rep)
  OScalar rep _ -> T.pack (show rep)
  ORecord con _ -> if T.null con.name then "object" else con.name
  OSum _ -> "sum"
  OList c -> "[" <> outlineLabel c <> "]"
  OOptional c -> outlineLabel c <> "?"
  OUnit -> "()"
  OChecked _ c -> outlineLabel c
  ODocumented _ c -> outlineLabel c

-- | Failures are discovered while compiling the entire structure, before values
-- are encoded. Paths identify fields, constructors and container elements.
data ProjectionError = ProjectionError {path :: [Text], problem :: Text}
  deriving (Eq, Show)

-- | Compile the default model representation, rejecting unsupported leaves and
-- lossy nullable combinations across the entire finite tree. Single positional
-- fields are transparent; any named zero-field Record becomes a string enum.
-- All-nullary sums use string enums; other sums use tagged objects.
compileModel :: Structure a -> Either ProjectionError (Codec a)
compileModel = compileWith (const Nothing)

-- | An open interpreter: override any typed node, delegate the remainder to the
-- model rules. An override owns both its schema and codec and must compile all
-- children it supports. The hook is consulted at EVERY node, including Named,
-- Mapped, Checked and Documented. Recursive overrides can call compileWith
-- themselves, or use compileFieldsWith / compileCaseWith for compound nodes.
-- Scalar matching can use eqTypeRep and HRefl, without unchecked casts.
compileWith ::
  (forall x. Structure x -> Maybe (Either ProjectionError (Codec x))) ->
  Structure a ->
  Either ProjectionError (Codec a)
compileWith override = go
 where
  at n = either (\(ProjectionError p e) -> Left (ProjectionError (n : p) e)) Right
  go :: Structure x -> Either ProjectionError (Codec x)
  go tree = case override tree of
    Just result -> result
    Nothing -> case tree of
      Scalar info mc -> maybe (Left (ProjectionError [] ("unsupported model scalar " <> info.name))) Right mc
      Named rep c -> reschema (titled (T.pack (show rep))) <$> go c
      Mapped to' from' c -> mapCodec to' from' <$> go c
      Checked rule ok c -> do
        codec <- go c
        let s = codec.schema
        pure $ Codec (s{checks = s.checks <> [rule]}) codec.encode $ \v -> do
          a <- codec.decode v
          if ok a then Right a else Left ("must be " <> rule)
      Documented d c -> reschema (documentedSchema d) <$> go c
      Unit -> Right (Codec (schemaOf SNull) (const Null) (\v -> case v of Null -> Right (); _ -> Left "expected null"))
      List c -> do
        inner <- at "[]" (go c)
        pure
          ( Codec
              (schemaOf (SArray inner.schema))
              (Array . map inner.encode)
              (\case Array vs -> traverse inner.decode vs; _ -> Left "expected a list")
          )
      Optional c -> do
        inner <- at "?" (go c)
        if acceptsNull inner.schema
          then Left (ProjectionError [] "model optional cannot distinguish nested nullable values")
          else
            pure
              ( Codec
                  (schemaOf (SNullable inner.schema))
                  (maybe Null inner.encode)
                  (\case Null -> Right Nothing; v -> Just <$> inner.decode v)
              )
      Record con fs -> do
        obj <- compileFieldsWith go fs
        let codec = C.record "" obj
        case (con.style, obj.fields) of
          (_, [])
            | not (T.null con.name) ->
                pure $
                  Codec
                    (schemaOf (SEnum [(con.name, Nothing)]))
                    (const (String con.name))
                    (\case String tag | tag == con.name -> codec.decode (Object []); _ -> Left "expected the constructor name")
          (PositionalFields, [Schema.Field n s _]) ->
            pure
              ( Codec
                  s
                  (\a -> case codec.encode a of Object kvs -> maybe Null id (lookup n kvs); _ -> Null)
                  (\v -> codec.decode (Object [(n, v)]))
              )
          _ -> pure codec
      Sum cs -> C.sumOf "" <$> traverse (compileCaseWith go) cs
  acceptsNull s = case s.shape of SNullable _ -> True; SNull -> True; _ -> False

-- Apply field annotations before choosing a layout: an adapter may put fields
-- inside a payload object or use a completely different projected schema.
annotateField :: Text -> Text -> Structure a -> Maybe (Structure a)
annotateField n d = go
 where
  go :: Structure x -> Maybe (Structure x)
  go = \case
    Named rep c -> Named rep <$> go c
    Mapped f g c -> Mapped f g <$> go c
    Checked rule ok c -> Checked rule ok <$> go c
    Documented doc c -> Documented doc <$> go c
    Record con fs | contains fs -> Just (Record con (update fs))
    Sum cs
      | any (\(Case _ _ _ _ fs) -> contains fs) cs ->
          Just (Sum [Case con doc match inject (update fs) | Case con doc match inject fs <- cs])
    _ -> Nothing
  contains :: Fields r x -> Bool
  contains = \case
    Pure _ -> False
    Apply f x -> contains f || contains x
    Field name _ _ _ -> name == n
  update :: Fields r x -> Fields r x
  update = \case
    Pure x -> Pure x
    Apply f x -> Apply (update f) (update x)
    Field name doc c get -> Field name (if name == n then Just d else doc) c get

-- | Compile an applicative field structure using the supplied child interpreter.
-- The resulting ObjectCodec can be assembled with Agentic.Codec builders or
-- used directly for a different object layout. Nullable fields use that codec's
-- schema; an adapter can override requiredness/absence policy if necessary.
compileFieldsWith ::
  (forall x. Structure x -> Either ProjectionError (Codec x)) ->
  Fields r a ->
  Either ProjectionError (C.ObjectCodec r a)
compileFieldsWith interpretField = go
 where
  go :: Fields r x -> Either ProjectionError (C.ObjectCodec r x)
  go = \case
    Pure a -> Right (pure a)
    Apply f x -> (<*>) <$> go f <*> go x
    Field n d c get -> C.requiredWith n d <$> atPath n (interpretField c) <*> pure get

-- | Compile a case with total matching and reconstruction. This does not choose
-- the enclosing sum's wire layout; the caller owns that decision.
compileCaseWith ::
  (forall x. Structure x -> Either ProjectionError (Codec x)) ->
  Case a ->
  Either ProjectionError (C.CaseCodec a)
compileCaseWith interpretField (Case con d match inject fs) = do
  obj <- atPath con.name (compileFieldsWith interpretField fs)
  pure $ C.CaseCodec con.name d obj.fields (fmap obj.encode . match) (fmap inject . obj.decode)

atPath :: Text -> Either ProjectionError a -> Either ProjectionError a
atPath n = either (\(ProjectionError p e) -> Left (ProjectionError (n : p) e)) Right

instance Contract Text where
  contract =
    scalar (ScalarInfo "Data.Text.Text" []) $
      Just $
        C.primitive (SString Nothing) String (\case String s -> Right s; v -> C.mismatch "text" v)
instance Contract Bool where
  contract =
    scalar (ScalarInfo "Prelude.Bool" []) $
      Just $
        C.primitive SBool Bool (\case Bool b -> Right b; v -> C.mismatch "a boolean" v)
instance Contract Integer where
  contract =
    scalar (ScalarInfo "Prelude.Integer" []) $
      Just $
        C.primitive SInteger Integer (\case Integer n -> Right n; Number d | not (isNaN d || isInfinite d), d == fromInteger (round d) -> Right (round d); v -> C.mismatch "an integer" v)
instance Contract Int where
  contract =
    scalar (ScalarInfo "Prelude.Int" []) $
      Just $
        C.primitive
          SInteger
          (Integer . toInteger)
          ( \case
              Integer n | inRange n -> Right (fromInteger n)
              Number d | not (isNaN d || isInfinite d), let n = round d, d == fromInteger n, inRange n -> Right (fromInteger n)
              v -> C.mismatch "an Int" v
          )
   where
    inRange n = n >= toInteger (minBound :: Int) && n <= toInteger (maxBound :: Int)
instance Contract Double where
  contract =
    scalar (ScalarInfo "Prelude.Double" []) $
      Just $
        C.primitive SNumber Number (\case Number d -> Right d; Integer n -> Right (fromInteger n); v -> C.mismatch "a number" v)
instance Contract () where contract = Unit
instance Contract a => Contract [a] where contract = List contract
instance Contract a => Contract (Maybe a) where contract = Optional contract
instance (Contract a, Contract b) => Contract (a, b) where
  contract = documented "A pair" $ Record (Constructor "(,)" PositionalFields) ((,) <$> required "_1" "" fst <*> required "_2" "" snd)
instance (Contract a, Contract b, Contract c) => Contract (a, b, c) where
  contract = documented "A triple" $ Record (Constructor "(,,)" PositionalFields) ((,,) <$> required "_1" "" (\(a, _, _) -> a) <*> required "_2" "" (\(_, b, _) -> b) <*> required "_3" "" (\(_, _, c) -> c))

-- | Derive from Generic while preserving nominal/applied identity, constructor
-- style and field order. Model compilation alone decides whether a positional
-- wrapper is transparent. The resulting contract tree must be finite.
genericContract :: forall a. (Generic a, Typeable a, GContract (Rep a)) => Structure a
genericContract = nominal (mapStructure to from (gcontract @(Rep a)))

-- | Constraint supporting generic derivation. Use genericContract rather than
-- depending on the representation traversal itself. Trees must be finite;
-- arbitrary recursive contracts are not supported by discovery or compilation.
class GContract (f :: Type -> Type) where
  -- | Describe a datatype representation without a nominal outer wrapper.
  gcontract :: Structure (f p)

instance GContract f => GContract (M1 D d f) where
  gcontract = mapStructure M1 unM1 (gcontract @f)
instance (GHC.Generics.Constructor c, GFields f) => GContract (M1 C c f) where
  gcontract = Record (genericConstructor @c) (M1 <$> lmapObject unM1 (snd (gfields @f 1)))
instance (GCases f, GCases g) => GContract (f :+: g) where
  gcontract = Sum (gcases @(f :+: g))
instance GContract V1 where gcontract = Sum []

-- | Generic sum traversal retaining constructor order and typed payloads.
class GCases (f :: Type -> Type) where
  -- | Describe the cases of a generic sum.
  gcases :: [Case (f p)]

instance GCases V1 where gcases = []
instance (GCases f, GCases g) => GCases (f :+: g) where
  gcases =
    map (inject L1 (\case L1 x -> Just x; _ -> Nothing)) (gcases @f)
      <> map (inject R1 (\case R1 x -> Just x; _ -> Nothing)) (gcases @g)
   where
    inject wrap unwrap (Case con d match construct fs) =
      Case con d (\y -> unwrap y >>= match) (wrap . construct) fs
instance (GHC.Generics.Constructor c, GFields f) => GCases (M1 C c f) where
  gcases = [Case (genericConstructor @c) Nothing (Just . unM1) M1 (snd (gfields @f 1))]

genericConstructor :: forall (c :: Meta). GHC.Generics.Constructor c => Constructor
genericConstructor =
  Constructor
    (T.pack (conName (undefined :: M1 C c U1 ())))
    (if conIsRecord (undefined :: M1 C c U1 ()) then RecordFields else PositionalFields)

-- | Generic field traversal retaining selector names and declaration order.
class GFields (f :: Type -> Type) where
  -- | Number positional fields from the supplied index, returning the next index.
  gfields :: Int -> (Int, Fields (f p) (f p))

instance GFields U1 where gfields n = (n, pure U1)
instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields n =
    let (n1, a) = gfields @f n; (n2, b) = gfields @g n1
     in (n2, (:*:) <$> lmapObject (\(x :*: _) -> x) a <*> lmapObject (\(_ :*: y) -> y) b)
instance (Selector s, Contract a) => GFields (M1 S s (K1 i a)) where
  gfields n = (n + 1, M1 . K1 <$> Field name Nothing contract (unK1 . unM1))
   where
    selector = selName (undefined :: M1 S s (K1 i a) ())
    name = if null selector then "_" <> T.pack (show n) else T.pack selector

-- | An enumeration value with its model-facing label and optional description.
data Option a = Option
  { value :: a
  , label :: Text
  , doc :: Maybe Text
  }

-- | A documented enumeration in declaration order (lowest to highest for scores).
data OptionSet a = OptionSet
  { doc :: Maybe Text
  , options :: [Option a]
  -- ^ In order. For a score, this is the level order, lowest first.
  }

-- | Types whose values are a fixed, ordered list of described options. Jev's
-- @choice@ and @score@ need one.
class Options a where
  options :: OptionSet a
  default options :: (Generic a, GEnum (Rep a), Show a) => OptionSet a
  options = OptionSet Nothing [Option v (showLabel v) Nothing | v <- map to (genum @(Rep a))]

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

-- | Describe an Options type as a sum of documented nullary cases.
enumeration :: forall a. (Options a, Eq a) => Structure a
enumeration =
  documented (maybe "" id set.doc) $
    Sum
      [ Case (Constructor o.label PositionalFields) o.doc (\a -> if a == o.value then Just () else Nothing) (const o.value) (pure ())
      | o <- set.options
      ]
 where
  set = options @a

-- | Constraint supporting generic Options derivation.
class GEnum (f :: Type -> Type) where
  -- | Enumerate nullary constructors in declaration order.
  genum :: [f p]

instance GEnum f => GEnum (M1 D d f) where
  genum = map M1 genum

instance (GEnum f, GEnum g) => GEnum (f :+: g) where
  genum = map L1 genum <> map R1 genum

instance GEnum (M1 C c U1) where
  genum = [M1 U1]
