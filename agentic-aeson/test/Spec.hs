{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, NoFieldSelectors #-}
module Main (main) where

import Agentic
import Agentic.Aeson (fromAeson)
import Data.Aeson qualified as J
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

-- JSON objects are unordered. Arrays (including mappings and label lines)
-- retain their order, so only object fields are normalised for comparison.
normalise :: Value -> Value
normalise (Object fs) = Object (sortOn fst [(k,normalise v) | (k,v) <- fs])
normalise (Array xs) = Array (map normalise xs)
normalise v = v

main :: IO ()
main = hspec $ H.describe "flow graph JSON round trip" $ do
  let self = recursiveTool $ \s -> tool @Text @Text "same\n\"\\雪" "" (draftWith [s] "again")
      recursive = each (draftWith @Text @Text @IO [self,self] "start")
      routing = ((returnA &&& act pure) ||| (returnA &&& act pure)) >>> arr fst
                >>> repeatUntil (>=3) (act (pure . (+1))) :: Agentic IO (Either Int Int) Int
      encode flow = flowGraphValue (flowGraph (either (error . show) id (describe flow)))
  mapM_ (\(name,value) -> it name $ do
    let parsed = J.eitherDecodeStrict (TE.encodeUtf8 (renderJson value))
    fmap (normalise . fromAeson) parsed `shouldBe` Right (normalise value))
    [("preserves recursive references, reused bindings and loop bodies",encode recursive)
    ,("preserves routing junctions, selections, hidden aliases and repeat edges",encode routing)]
