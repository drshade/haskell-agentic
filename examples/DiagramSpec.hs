-- Exact rendering fixtures. Regenerate with:
-- cabal test agentic-examples:diagram-test --test-options="--write diagrams"
module Main (main) where

import Agentic
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.List (nub)
import KitchenSink (kitchenSink)
import System.Environment (getArgs)

leftAfterFanout :: Agentic IO Int (Either (Int,Int) Int)
leftAfterFanout = (act pure `named` "source one" &&& act pure `named` "source two")
  >>> arr Left >>> left (act pure `named` "handle pair")

choiceWithBypasses :: Agentic IO (Either Int Int) (Int,Int)
choiceWithBypasses = (returnA &&& act pure `named` "x") ||| (returnA &&& act pure `named` "y")

downstreamFork :: Agentic IO (Either Int Int) (Int,Int)
downstreamFork = (returnA ||| returnA) >>> (act pure `named` "f" &&& act pure `named` "g")

nestedChoice :: Agentic IO (Either (Either Int Int) Int) Int
nestedChoice = (returnA ||| act pure `named` "inner") ||| returnA

choiceInRepeat :: Agentic IO Int Int
choiceInRepeat = repeatUntil (>=3) (arr Left >>> (arr (+1) ||| returnA))

main :: IO ()
main = do
  args <- getArgs
  let check name description = do
        d <- either throwIO pure description
        let graph = flowGraph d
            nodes = flatten graph.items
            diamonds = [i | (i,Just DecisionNode) <- nodes]
            dots = [i | (i,Just JunctionNode) <- nodes]
            outgoing i = [e | e <- graph.edges, e.from == i]
            selected = [e | e <- graph.edges, not (null e.selections)]
            validDecision i = length (outgoing i) == 2
              && all ((==1) . length . (.selections)) (outgoing i)
              && all (`elem` map (.label) (outgoing i)) [Just "left",Just "right"]
            validSelection e = case e.selections of
              [EdgeId address _] -> e.from `elem` diamonds && lookup address graph.addressItems == Just e.from
              _ -> False
        unless (all validDecision diamonds && all validSelection selected
          && all ((`notElem` dots) . snd) graph.addressItems
          && length (map fst nodes) == length (nub (map fst nodes)))
          (fail (name <> ": invalid decision routing or item identity"))
        let outputs = [("mmd",mermaid d),("dot",dot d)]
        mapM_ (\(extension,actual) -> case args of
          ["--write",directory] -> T.writeFile (directory <> "/" <> name <> "." <> extension) actual
          [] -> do
            expected <- T.readFile ("diagrams/" <> name <> "." <> extension)
            unless (actual == expected) (fail (name <> "." <> extension <> " differs; inspect the rendering before regenerating"))
          _ -> fail "usage: diagram-test [--write directory]") outputs
  check "kitchen-sink" (describe kitchenSink)
  check "left-after-fanout" (describe leftAfterFanout)
  check "choice-with-bypasses" (describe choiceWithBypasses)
  check "downstream-fork" (describe downstreamFork)
  check "nested-choice" (describe nestedChoice)
  check "choice-in-repeat" (describe choiceInRepeat)

flatten :: [Item] -> [(Text,Maybe NodeKind)]
flatten = concatMap (\case
  ItemNode i kind _ -> [(i,Just kind)]
  ItemBox i _ children -> (i,Nothing):flatten children)
