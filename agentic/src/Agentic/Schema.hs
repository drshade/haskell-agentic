-- | The schema of a 'Agentic.Contract.Contract'. It's richer than any one
-- provider's wire format; provider packages lower it to what they accept.
module Agentic.Schema
  ( Schema (..)
  , Shape (..)
  , Field (..)
  , Variant (..)
  , Format (..)
  , schemaOf
  , documentedSchema
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
  { name :: Text
  , schema :: Schema
  , required :: Bool
  }
  deriving (Eq, Show)

data Variant = Variant
  { tag :: Text
  , doc :: Maybe Text
  , fields :: [Field]
  }
  deriving (Eq, Show)

data Format = DateTime | Date | Email | Uri | Uuid
  deriving (Eq, Show)

schemaOf :: Shape -> Schema
schemaOf = Schema Nothing Nothing []

documentedSchema :: Text -> Schema -> Schema
documentedSchema d (Schema t _ cs sh) = Schema t (Just d) cs sh

-- | Name the schema's type, unless it already has a name.
titled :: Text -> Schema -> Schema
titled t s = s {title = maybe (Just t) Just s.title}

-- | A short label for display, e.g. in 'Agentic.Describe.describe'.
typeLabel :: Schema -> Text
typeLabel s = maybe (structural s.shape) id s.title
  where
    structural = \case
      SObject _ -> "object"
      SSum [] -> "sum"
      SSum vs -> "sum of " <> joinTags (map (.tag) vs)
      SEnum ls -> "one of " <> joinTags (map fst ls)
      SArray inner -> "[" <> typeLabel inner <> "]"
      SNullable inner -> typeLabel inner <> "?"
      SString _ -> "text"
      SInteger -> "integer"
      SNumber -> "number"
      SBool -> "bool"
      SNull -> "()"
    joinTags ts = mconcat (zipWith (<>) ("" : repeat "|") ts)
