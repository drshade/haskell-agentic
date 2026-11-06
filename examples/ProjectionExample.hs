{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A standalone adapter example. All storage conventions are local to this
-- module; Agentic owns neither the identifier nor this storage protocol.
module ProjectionExample where

import Agentic hiding (model)
import Agentic.Codec (Case (..), ObjectCodec (..))
import Agentic.Contract (ContractTree (..))
import Agentic.Schema (Field (..), Shape (..), Variant (..))
import Data.Fixed (E2, Fixed (..), showFixed)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Type.Reflection

newtype FactId = FactId Text deriving (Eq, Show)
instance Contract FactId where
  contract = scalar (ScalarInfo "example.FactId" []) (Just identifierCodec)

identifierCodec :: Codec FactId
identifierCodec = Codec (schemaOf (SString Nothing)) (\(FactId t) -> String t) $ \case
  String t | not (T.null t) -> Right (FactId t)
  _ -> Left "expected a nonempty identifier"

-- An adapter supplied once for a library type, not for every containing record.
instance Contract (Fixed E2) where
  contract = scalar (ScalarInfo "Data.Fixed.Fixed" [("resolution", "100")]) (Just decimalCodec)

decimalCodec :: Codec (Fixed E2)
decimalCodec = Codec (schemaOf (SString Nothing)) (String . T.pack . showFixed False) $ \case
  String t -> do
    let (sign, unsigned) = case T.stripPrefix "-" t of Just u -> (-1, u); Nothing -> (1, t)
    case T.splitOn "." unsigned of
      [whole, frac] | digits whole && digits frac && T.length frac == 2 ->
        case (readMaybe (T.unpack whole), readMaybe (T.unpack frac)) of
          (Just w, Just f) ->
            let result = MkFixed (sign * (w * 100 + f))
             in if T.pack (showFixed False result) == t then Right result else Left "noncanonical decimal"
          _ -> Left "invalid decimal"
      _ -> Left "expected decimal text with two fractional digits"
  _ -> Left "expected exact decimal text"
 where
  digits t = not (T.null t) && T.all (\c -> c >= '0' && c <= '9') t

data Sample = Sample {key :: FactId, amount :: Fixed E2, confidence :: Probability}
  deriving (Generic, Contract, Eq, Show)
data Box a = Box {value :: a} deriving (Generic, Contract, Eq, Show)
data Payload = Empty | Amount (Fixed E2) | Confidence Probability
  deriving (Generic, Contract, Eq, Show)

sample :: Sample
sample = Sample (FactId "sample-1") (MkFixed 1234) (fromBasisPoints 8500)

integerText :: Codec Integer
integerText = Codec (schemaOf (SString Nothing)) (String . T.pack . show) $ \case
  String t | Just n <- readMaybe (T.unpack t), T.pack (show (n :: Integer)) == t -> Right n
  _ -> Left "expected a canonical integer string"

probabilityStorage :: Codec Probability
probabilityStorage = Codec integerText.schema (integerText.encode . toInteger . basisPoints) $ \v -> do
  n <- integerText.decode v
  if n < 0 || n > 10000
    then Left "basis points must be between 0 and 10000"
    else Right (fromBasisPoints (fromInteger n))

-- | An independent projection for upstream and downstream leaves. Named types,
-- mappings, validation and lists reuse the compiler's traversal.
-- Overrides are typed, and return the schema together with its matching codec.
storage :: ContractTree a -> Either ProjectionError (Codec a)
storage = compileWith storageRules

storageRules :: forall a. ContractTree a -> Maybe (Either ProjectionError (Codec a))
storageRules = \case
  Scalar info modelDefault -> Just $ case () of
    _
      | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @Probability) -> Right probabilityStorage
      | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @(Fixed E2)) -> Right decimalCodec
      | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @FactId) -> Right identifierCodec
      | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @Integer) -> Right integerText
      | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @Int) ->
          Right $
            Codec
              integerText.schema
              (integerText.encode . toInteger)
              ( \v -> do
                  n <- integerText.decode v
                  if n < toInteger (minBound :: Int) || n > toInteger (maxBound :: Int)
                    then Left "Int out of range"
                    else Right (fromInteger n)
              )
      | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @Text) -> defaultCodec info modelDefault
      | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @Bool) -> defaultCodec info modelDefault
      | otherwise -> Left (ProjectionError [] ("unsupported storage scalar " <> info.name))
  Optional child -> Just $ do
    c <- storage child
    pure $
      Codec
        (schemaOf (SSum [Variant "None" Nothing [], Variant "Some" Nothing [Field "value" c.schema True]]))
        (\case Nothing -> Object [("tag", String "None")]; Just x -> Object [("tag", String "Some"), ("value", c.encode x)])
        ( \case
            Object [("tag", String "None")] -> Right Nothing
            Object fields | lookup "tag" fields == Just (String "Some"), Just v <- lookup "value" fields -> Just <$> c.decode v
            _ -> Left "expected a None/Some tagged optional"
        )
  Record con fields -> Just $ do
    fs <- compileFieldsWith storage fields
    let tagged = con.style == PositionalFields && not (null fs.fields)
        tagField = Field "tag" (schemaOf (SEnum [(con.name, Nothing)])) True
    pure $
      Codec
        (schemaOf (SObject (if tagged then tagField : fs.fields else fs.fields)))
        (\a -> Object ((if tagged then [("tag", String con.name)] else []) <> fs.encode a))
        ( \case
            Object values | not tagged || lookup "tag" values == Just (String con.name) -> fs.decode values
            _ -> Left "expected a record with its constructor tag"
        )
  Sum cases -> Just $ do
    compiled <- traverse (compileCaseWith storage) cases
    pure $
      Codec
        (schemaOf (SSum [Variant c.tag c.doc (if null c.fields then [] else [Field "payload" (schemaOf (SObject c.fields)) True]) | c <- compiled]))
        ( \a -> case [(c.tag, values) | c <- compiled, Just values <- [c.encode a]] of
            (tag, values) : _ -> Object ([("tag", String tag)] <> if null values then [] else [("payload", Object values)])
            [] -> Null
        )
        ( \case
            Object values | Just (String tag) <- lookup "tag" values ->
              case [c | c <- compiled, c.tag == tag] of
                c : _
                  | null c.fields -> c.decode []
                  | Just (Object payload) <- lookup "payload" values -> c.decode payload
                _ -> Left "unknown constructor or missing payload"
            _ -> Left "expected a tagged sum"
        )
  Unit -> Just $ Right $ Codec (schemaOf (SObject [])) (const (Object [])) $ \case
    Object [] -> Right ()
    _ -> Left "expected an empty object"
  _ -> Nothing
 where
  defaultCodec info = maybe (Left (ProjectionError [] ("unsupported storage scalar " <> info.name))) Right

main :: IO ()
main = do
  print (descriptor (contract @Sample))
  case (storage (contract @Sample), compileModel (contract @Sample)) of
    (Right stored, Right model) -> do
      print (renderJson (stored.encode sample))
      print (renderJson (model.encode sample))
      print (stored.decode (stored.encode sample) == Right sample)
    (Left e, _) -> fail (show e)
    (_, Left e) -> fail (show e)
