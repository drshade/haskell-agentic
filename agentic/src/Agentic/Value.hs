-- | A small JSON-shaped value. The core owns this type so that it depends only on
-- @base@ and @text@; provider packages convert to and from their wire libraries.
module Agentic.Value
  ( Value (..)
  , renderJson
  , lookupField
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

data Value
  = Null
  | Bool Bool
  | Integer Integer
  | Number Double
  | String Text
  | Array [Value]
  | Object [(Text, Value)]
    -- ^ Fields keep their order, so rendering is stable (useful for caching).
  deriving (Eq, Ord, Show)

-- | Render as compact JSON text.
renderJson :: Value -> Text
renderJson = \case
  Null -> "null"
  Bool True -> "true"
  Bool False -> "false"
  Integer n -> T.pack (show n)
  Number d -> T.pack (show d)
  String s -> quote s
  Array vs -> "[" <> T.intercalate "," (map renderJson vs) <> "]"
  Object kvs -> "{" <> T.intercalate "," [quote k <> ":" <> renderJson v | (k, v) <- kvs] <> "}"

quote :: Text -> Text
quote s = "\"" <> T.concatMap escape s <> "\""
  where
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c | c < ' ' -> T.pack ("\\u" <> pad (showHex (fromEnum c) ""))
        | otherwise -> T.singleton c
    pad h = replicate (4 - length h) '0' <> h

lookupField :: Text -> [(Text, Value)] -> Maybe Value
lookupField = lookup
