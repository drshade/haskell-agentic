{-# LANGUAGE GADTs #-}
module Agentic.Schema
  ( SchemaFormat(..)
  ) where

import Autodocodec (HasCodec)
import Data.Aeson (FromJSON, ToJSON)
import Dhall (FromDhall, ToDhall)

-- | GADT capturing which schema format to use and its required constraints.
-- Use with 'extractWith' to explicitly select a schema format.
data SchemaFormat a where
  Dhall :: (FromDhall a, ToDhall a) => SchemaFormat a
  Json  :: (FromJSON a, ToJSON a, HasCodec a) => SchemaFormat a
