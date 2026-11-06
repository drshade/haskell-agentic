{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | One typed structural contract, interpreted into independently chosen codecs.
module Agentic.Contract
  ( Contract (..)
  , ContractTree (..)
  , Fields (..)
  , Case (..)
  , Constructor (..)
  , FieldStyle (..)
  , ScalarInfo (..)
  , Descriptor (..)
  , FieldDescriptor (..)
  , CaseDescriptor (..)
  , Codec (..)
  , ProjectionError (..)
  , compileModel
  , compileWith
  , compileFieldsWith
  , compileCaseWith
  , descriptor
  , descriptorLabel
  , scalar
  , namedContract
  , mapContract
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
import Agentic.Schema
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
data ContractTree a where
  Scalar :: Typeable a => ScalarInfo -> Maybe (Codec a) -> ContractTree a
  Named :: TypeRep a -> ContractTree a -> ContractTree a
  Record :: Constructor -> Fields a a -> ContractTree a
  Sum :: [Case a] -> ContractTree a
  List :: ContractTree a -> ContractTree [a]
  Optional :: ContractTree a -> ContractTree (Maybe a)
  Unit :: ContractTree ()
  Mapped :: (a -> b) -> (b -> a) -> ContractTree a -> ContractTree b
  Checked :: Text -> (a -> Bool) -> ContractTree a -> ContractTree a
  Documented :: Text -> ContractTree a -> ContractTree a
  FieldDoc :: Text -> Text -> ContractTree a -> ContractTree a

-- | Applicative fields retain getters, reconstruction, and declaration order.
data Fields r a where
  PureFields :: a -> Fields r a
  ApplyFields :: Fields r (a -> b) -> Fields r a -> Fields r b
  FieldNode :: Text -> Maybe Text -> ContractTree a -> (r -> a) -> Fields r a

instance Functor (Fields r) where
  fmap f x = ApplyFields (PureFields f) x
instance Applicative (Fields r) where
  pure = PureFields
  (<*>) = ApplyFields

-- | Adapt the input seen by field getters, retaining their order and reconstruction.
lmapObject :: (s -> r) -> Fields r a -> Fields s a
lmapObject f = \case
  PureFields a -> PureFields a
  ApplyFields g x -> ApplyFields (lmapObject f g) (lmapObject f x)
  FieldNode n d c get -> FieldNode n d c (get . f)

-- | A sum case exposes its payload only after successful matching. The payload
-- type is existential: generic field getters cannot be applied to another arm.
data Case a where
  Case :: Constructor -> Maybe Text -> (a -> Maybe p) -> (p -> a) -> Fields p p -> Case a

-- | A single structural authority for discovery and independently compiled codecs.
-- Derive with Generic, or build a tree explicitly with record, sumOf and scalar.
class Contract a where
  -- | The typed description; no storage or model projection is selected yet.
  contract :: ContractTree a
  default contract :: (Generic a, Typeable a, GContract (Rep a)) => ContractTree a
  contract = genericContract

-- | Define an identifiable semantic leaf with optional default model behavior.
-- Other interpreters can supply codecs even when that default is absent.
scalar :: Typeable a => ScalarInfo -> Maybe (Codec a) -> ContractTree a
scalar = Scalar
-- | Preserve a type's resolved nominal identity and applied arguments. Useful
-- around handwritten contracts and mappings; generic derivation adds it itself.
namedContract :: forall a. Typeable a => ContractTree a -> ContractTree a
namedContract = Named (typeRep @a)

-- | Change the Haskell representation without discarding the inner structure.
-- The functions should be inverse on supported values. Wrap with namedContract
-- when the outer type has a nominal identity that discovery must preserve.
mapContract :: (a -> b) -> (b -> a) -> ContractTree a -> ContractTree b
mapContract = Mapped

-- | Close applicative fields into a documented record description. Use Named
-- (or namedContract) when a handwritten record also needs nominal identity.
record :: Text -> Fields a a -> ContractTree a
record d = documented d . Record (Constructor "" RecordFields)
-- | A named, documented field using its type's contract and a getter.
required :: Contract a => Text -> Text -> (r -> a) -> Fields r a
required n d = requiredWith n (nonEmpty d) contract
-- | A named field with an explicit structural description. Missing-field
-- behavior is chosen by the projection; the model permits absent Maybe fields.
requiredWith :: Text -> Maybe Text -> ContractTree a -> (r -> a) -> Fields r a
requiredWith = FieldNode
-- | A Maybe field. Models use null/absence; other projections may use tags.
optional :: Contract a => Text -> Text -> (r -> Maybe a) -> Fields r (Maybe a)
optional = required
-- | A handwritten sum case with a predicate and applicative fields. Getters
-- must be defined for every value accepted by the predicate. For typed payload
-- matching and injection, use the existential Case constructor directly.
constructor :: Text -> Text -> (a -> Bool) -> Fields a a -> Case a
constructor n d matches = Case (Constructor n RecordFields) (nonEmpty d) (\a -> if matches a then Just a else Nothing) id
-- | Describe an ordered set of sum cases. The interpreter chooses their layout.
sumOf :: Text -> [Case a] -> ContractTree a
sumOf d = documented d . Sum
-- | Describe the whole contract, without selecting a representation.
documented :: Text -> ContractTree a -> ContractTree a
documented "" = id
documented d = Documented d
-- | Describe one field of a record or each matching sum arm. Annotation is
-- applied before projection so it follows layout changes. A missing field is
-- reported when compiling; discovery remains a total view of finite trees.
field :: Text -> Text -> ContractTree a -> ContractTree a
field = FieldDoc
-- | Share a semantic predicate across projections. Decoders reject values that
-- fail it; model schemas also carry the rule as explanatory text.
checked :: Text -> (a -> Bool) -> ContractTree a -> ContractTree a
checked = Checked
-- | Require a decoded value to lie within the inclusive bounds.
between :: (Ord a, Show a) => a -> a -> ContractTree a -> ContractTree a
between lo hi = checked ("between " <> T.pack (show lo) <> " and " <> T.pack (show hi)) (\a -> a >= lo && a <= hi)
nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

-- | Pure discovery data derived from the tree, independent of any codec.
data Descriptor
  = ScalarDescriptor SomeTypeRep ScalarInfo
  | NamedDescriptor SomeTypeRep Descriptor
  | RecordDescriptor Constructor [FieldDescriptor]
  | SumDescriptor [CaseDescriptor]
  | ListDescriptor Descriptor
  | OptionalDescriptor Descriptor
  | UnitDescriptor
  | MappedDescriptor Descriptor
  | CheckedDescriptor Text Descriptor
  | DocumentedDescriptor Text Descriptor
  deriving (Eq, Show)

-- | A field's name, optional documentation, and structural descriptor.
data FieldDescriptor = FieldDescriptor Text (Maybe Text) Descriptor deriving (Eq, Show)
-- | Constructor metadata, optional documentation, and fields in declaration order.
data CaseDescriptor = CaseDescriptor Constructor (Maybe Text) [FieldDescriptor] deriving (Eq, Show)

-- | Discover a finite contract without compiling or invoking any codec.
-- Functions and predicates remain in the tree rather than this pure view.
descriptor :: forall a. ContractTree a -> Descriptor
descriptor = \case
  Scalar info _ -> ScalarDescriptor (SomeTypeRep (typeRep @a)) info
  Named rep c -> NamedDescriptor (SomeTypeRep rep) (descriptor c)
  Record con fs -> RecordDescriptor con (describeFields fs)
  Sum cs -> SumDescriptor [CaseDescriptor con d (describeFields fs) | Case con d _ _ fs <- cs]
  List c -> ListDescriptor (descriptor c)
  Optional c -> OptionalDescriptor (descriptor c)
  Unit -> UnitDescriptor
  Mapped _ _ c -> MappedDescriptor (descriptor c)
  Checked rule _ c -> CheckedDescriptor rule (descriptor c)
  Documented d c -> DocumentedDescriptor d (descriptor c)
  -- Discovery stays total for missing field annotations; compilation reports them.
  FieldDoc n d c -> maybe (descriptor c) descriptor (annotateField n d c)

describeFields :: Fields r a -> [FieldDescriptor]
describeFields = \case
  PureFields _ -> []
  ApplyFields f x -> describeFields f <> describeFields x
  FieldNode n d c _ -> [FieldDescriptor n d (descriptor c)]

-- | A concise display label. Scalar identity strings remain available separately.
descriptorLabel :: Descriptor -> Text
descriptorLabel = \case
  NamedDescriptor rep _ -> T.pack (show rep)
  ScalarDescriptor rep _ -> T.pack (show rep)
  RecordDescriptor con _ -> if T.null con.name then "object" else con.name
  SumDescriptor _ -> "sum"
  ListDescriptor c -> "[" <> descriptorLabel c <> "]"
  OptionalDescriptor c -> descriptorLabel c <> "?"
  UnitDescriptor -> "()"
  MappedDescriptor c -> descriptorLabel c
  CheckedDescriptor _ c -> descriptorLabel c
  DocumentedDescriptor _ c -> descriptorLabel c

-- | Failures are discovered while compiling the entire structure, before values
-- are encoded. Paths identify fields, constructors and container elements.
data ProjectionError = ProjectionError {path :: [Text], problem :: Text}
  deriving (Eq, Show)

-- | Compile the default model representation, rejecting unsupported leaves and
-- lossy nullable combinations across the entire finite tree. Single positional
-- fields are transparent; any named zero-field Record becomes a string enum.
-- All-nullary sums use string enums; other sums use tagged objects.
compileModel :: ContractTree a -> Either ProjectionError (Codec a)
compileModel = compileWith (const Nothing)

-- | An open interpreter: override any typed node, delegate the remainder to the
-- model rules. An override owns both its schema and codec and must compile all
-- children it supports. The hook is consulted at EVERY node, including Named,
-- Mapped, Checked and Documented. Recursive overrides can call compileWith
-- themselves, or use compileFieldsWith / compileCaseWith for compound nodes.
-- Scalar matching can use eqTypeRep and HRefl, without unchecked casts.
compileWith ::
  (forall x. ContractTree x -> Maybe (Either ProjectionError (Codec x))) ->
  ContractTree a ->
  Either ProjectionError (Codec a)
compileWith override = go
 where
  at n = either (\(ProjectionError p e) -> Left (ProjectionError (n : p) e)) Right
  go :: ContractTree x -> Either ProjectionError (Codec x)
  go tree = case override tree of
    Just result -> result
    Nothing -> case tree of
      Scalar info mc -> maybe (Left (ProjectionError [] ("unsupported model scalar " <> info.name))) Right mc
      Named rep c -> reschema (titled (T.pack (show rep))) <$> go c
      Mapped to' from' c -> mapCodec to' from' <$> go c
      Checked rule ok c -> C.checked rule ok <$> go c
      Documented d c -> C.documented d <$> go c
      FieldDoc n d c -> case annotateField n d c of
        Just annotated -> go annotated
        Nothing -> Left (ProjectionError [n] "no such field")
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
          (PositionalFields, [Field n s _]) ->
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
annotateField :: Text -> Text -> ContractTree a -> Maybe (ContractTree a)
annotateField n d = go
 where
  go :: ContractTree x -> Maybe (ContractTree x)
  go = \case
    Named rep c -> Named rep <$> go c
    Mapped f g c -> Mapped f g <$> go c
    Checked rule ok c -> Checked rule ok <$> go c
    Documented doc c -> Documented doc <$> go c
    FieldDoc name doc c -> annotateField name doc c >>= go
    Record con fs | contains fs -> Just (Record con (update fs))
    Sum cs
      | any (\(Case _ _ _ _ fs) -> contains fs) cs ->
          Just (Sum [Case con doc match inject (update fs) | Case con doc match inject fs <- cs])
    _ -> Nothing
  contains :: Fields r x -> Bool
  contains = \case
    PureFields _ -> False
    ApplyFields f x -> contains f || contains x
    FieldNode name _ _ _ -> name == n
  update :: Fields r x -> Fields r x
  update = \case
    PureFields x -> PureFields x
    ApplyFields f x -> ApplyFields (update f) (update x)
    FieldNode name doc c get -> FieldNode name (if name == n then Just d else doc) c get

-- | Compile an applicative field structure using the supplied child interpreter.
-- The resulting ObjectCodec can be assembled with Agentic.Codec builders or
-- used directly for a different object layout. Nullable fields use that codec's
-- schema; an adapter can override requiredness/absence policy if necessary.
compileFieldsWith ::
  (forall x. ContractTree x -> Either ProjectionError (Codec x)) ->
  Fields r a ->
  Either ProjectionError (C.ObjectCodec r a)
compileFieldsWith interpretField = go
 where
  go :: Fields r x -> Either ProjectionError (C.ObjectCodec r x)
  go = \case
    PureFields a -> Right (pure a)
    ApplyFields f x -> (<*>) <$> go f <*> go x
    FieldNode n d c get -> C.requiredWith n d <$> atPath n (interpretField c) <*> pure get

-- | Compile a case with total matching and reconstruction. This does not choose
-- the enclosing sum's wire layout; the caller owns that decision.
compileCaseWith ::
  (forall x. ContractTree x -> Either ProjectionError (Codec x)) ->
  Case a ->
  Either ProjectionError (C.Case a)
compileCaseWith interpretField (Case con d match inject fs) = do
  obj <- atPath con.name (compileFieldsWith interpretField fs)
  pure $ C.Case con.name d obj.fields (fmap obj.encode . match) (fmap inject . obj.decode)

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
genericContract :: forall a. (Generic a, Typeable a, GContract (Rep a)) => ContractTree a
genericContract = namedContract (mapContract to from (gcontract @(Rep a)))

-- | Constraint supporting generic derivation. Use genericContract rather than
-- depending on the representation traversal itself. Trees must be finite;
-- arbitrary recursive contracts are not supported by discovery or compilation.
class GContract (f :: Type -> Type) where
  -- | Describe a datatype representation without a nominal outer wrapper.
  gcontract :: ContractTree (f p)

instance GContract f => GContract (M1 D d f) where
  gcontract = mapContract M1 unM1 (gcontract @f)
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
  gfields n = (n + 1, M1 . K1 <$> FieldNode name Nothing contract (unK1 . unM1))
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
enumeration :: forall a. (Options a, Eq a) => ContractTree a
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
