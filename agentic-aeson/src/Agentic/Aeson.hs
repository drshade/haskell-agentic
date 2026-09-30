-- | Conversions between the core's 'Value' and aeson's.
module Agentic.Aeson
  ( toAeson
  , fromAeson
  ) where

import qualified Agentic.Value as A
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (floatingOrInteger)
import qualified Data.Vector as Vector

toAeson :: A.Value -> J.Value
toAeson = \case
  A.Null -> J.Null
  A.Bool b -> J.Bool b
  A.Integer n -> J.Number (fromInteger n)
  A.Number d -> J.Number (realToFrac d)
  A.String s -> J.String s
  A.Array vs -> J.Array (Vector.fromList (map toAeson vs))
  A.Object kvs -> J.Object (KeyMap.fromList [(Key.fromText k, toAeson v) | (k, v) <- kvs])

fromAeson :: J.Value -> A.Value
fromAeson = \case
  J.Null -> A.Null
  J.Bool b -> A.Bool b
  J.Number n -> either A.Number A.Integer (floatingOrInteger n)
  J.String s -> A.String s
  J.Array vs -> A.Array (map fromAeson (Vector.toList vs))
  J.Object o -> A.Object [(Key.toText k, fromAeson v) | (k, v) <- KeyMap.toList o]
