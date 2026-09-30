-- | The schema of a 'Agentic.Contract.Contract'. It's richer than any one
-- provider's wire format; provider packages lower it to what they accept.
module Agentic.Schema
  ( Schema (..)
  , Shape (..)
  , Field (..)
  , Variant (..)
  , Format (..)
  , schemaOf
  , documentSchema
  , typeLabel
  , titled
  ) where

import Data.Text (Text)

data Schema = Schema
  { title :: Maybe Text
    -- ^ The type's name, e.g. @Joke@. Providers use it to name schemas.
  , doc :: Maybe Text
  , checks :: [Text]
    -- ^ Constraints the wire schemas can't express, stated for the model and
    -- checked locally.
  , shape :: Shape
  }
  deriving (Eq, Show)

data Shape
  = SObject [Field]
  | SSum [Variant]
    -- ^ A tagged union: each variant is an object with a @tag@ field.
  | SEnum [(Text, Maybe Text)]
    -- ^ A choice of labels, each with an optional description.
  | SArray Schema
  | SNullable Schema
  | SString (Maybe Format)
  | SInteger
  | SNumber
  | SBool
  | SNull
  deriving (Eq, Show)

data Field = Field
  { fieldName :: Text
  , fieldSchema :: Schema
  , fieldRequired :: Bool
  }
  deriving (Eq, Show)

data Variant = Variant
  { variantTag :: Text
  , variantDoc :: Maybe Text
  , variantFields :: [Field]
  }
  deriving (Eq, Show)

data Format = DateTime | Date | Email | Uri | Uuid
  deriving (Eq, Show)

schemaOf :: Shape -> Schema
schemaOf = Schema Nothing Nothing []

documentSchema :: Text -> Schema -> Schema
documentSchema d s = s {doc = Just d}

-- | Name the schema's type, unless it already has a name.
titled :: Text -> Schema -> Schema
titled t s = s {title = maybe (Just t) Just (title s)}

-- | A short label for display, e.g. in 'Agentic.Describe.describe'.
typeLabel :: Schema -> Text
typeLabel s = maybe (structural (shape s)) id (title s)
  where
    structural = \case
      SObject _ -> "object"
      SSum [] -> "sum"
      SSum vs -> "sum of " <> joinTags (map variantTag vs)
      SEnum ls -> "one of " <> joinTags (map fst ls)
      SArray inner -> "[" <> typeLabel inner <> "]"
      SNullable inner -> typeLabel inner <> "?"
      SString _ -> "text"
      SInteger -> "integer"
      SNumber -> "number"
      SBool -> "bool"
      SNull -> "()"
    joinTags ts = mconcat (zipWith (<>) ("" : repeat "|") ts)
