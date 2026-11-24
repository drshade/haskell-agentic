-- Exact rendering fixtures. Regenerate with:
-- cabal test agentic-examples:diagram-test --test-options="--write diagrams"
module Main (main) where

import Agentic
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as T
import Data.List (nub)
import KitchenSink (kitchenSink)
import Dino (dinoProject)
import DinoSpec qualified
import EmailApproval qualified
import TicTacToe (game)
import Review (review)
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

-- The conversational reproduction from #19; describing it runs no model.
data Chat = Chat { outgoing :: Text, latestReply :: Text, finished :: Bool }

conversation :: Agentic IO () Text
conversation =
  (arr (const (Chat "Hello!" "" False)) `named` "Prepare greeting")
  >>> (repeatUntil (.finished) turn `named` "Chat until exit")
  >>> (arr (const "Finished") `named` "Finish conversation")
  where
    turn =
      ((arr id &&& (arr (.outgoing) >>> await @Text @Text))
        `named` "Ask a question and wait for reply")
      >>> (arr classify `named` "Is the reply exit?")
      >>> (stop ||| respond)
    classify (state, reply)
      | Text.toCaseFold (Text.strip reply) == "exit" = Left state
      | otherwise = Right (state { latestReply = reply })
    stop = arr (\state -> state { finished = True })
      `named` "Exit without calling the model"
    respond =
      (arr id &&& (arr (.latestReply) >>> answer))
      >>> (arr (\(state, reply) -> state { outgoing = reply })
        `named` "Remember answer for next turn")
    answer = draft @Text @Text "Reply briefly to the user's message."
      `named` "Generate answer"

splitLoop :: Agentic IO (Int,Int) (Int,Int)
splitLoop = repeatUntil ((>=3) . fst) (act pure *** act pure)
  >>> (act pure *** act pure)

nestedLoop :: Agentic IO Int Int
nestedLoop = repeatUntil (>=3) (repeatUntil (>=2) (act (pure . (+1))))

parallelLoop :: Agentic IO Int Int
parallelLoop = repeatUntil (>=3)
  ((act pure &&& act pure) >>> arr (uncurry max))

loopInChoice :: Agentic IO (Either Int Int) Int
loopInChoice = repeatUntil (>=3) (act (pure . (+1))) ||| returnA

emptyLoop :: Agentic IO Int Int
emptyLoop = repeatUntil (>=3) returnA

main :: IO ()
main = do
  args <- getArgs
  DinoSpec.check
  let check name description = do
        d <- either throwIO pure description
        let graph = flowGraph d
            nodes = flatten graph.items
            diamonds = [i | (i,Just DecisionNode) <- nodes]
            loops = [i | (i,Just LoopTestNode) <- nodes]
            validLoop i = all (\e -> null e.selections && e.label `elem` [Just "done",Just "continue"]) (outgoing i)
              && all (`elem` map (.label) (outgoing i)) [Just "done",Just "continue"]
            dots = [i | (i,Just JunctionNode) <- nodes]
            outgoing i = [e | e <- graph.edges, e.from == i]
            selected = [e | e <- graph.edges, not (null e.selections)]
            validDecision i = length (outgoing i) == 2
              && all ((==1) . length . (.selections)) (outgoing i)
              && all (`elem` map (.label) (outgoing i)) [Just "left",Just "right"]
            validSelection e = case e.selections of
              [EdgeId address _] -> e.from `elem` diamonds
                && lookup address graph.addressItems == Just e.from
              _ -> False
        unless (all validDecision diamonds && all validSelection selected && all validLoop loops
          && all ((`notElem` dots) . snd) graph.addressItems
          && length (map fst nodes) == length (nub (map fst nodes)))
          (fail (name <> ": invalid decision routing or item identity"))
        let outputs = [("mmd",mermaid d),("dot",dot d)] <> [("tree",renderTree d) | name == "dino"]
        mapM_ (\(extension,actual) -> case args of
          ["--write",directory] -> T.writeFile (directory <> "/" <> name <> "." <> extension) actual
          [] -> do
            expected <- T.readFile ("diagrams/" <> name <> "." <> extension)
            unless (actual == expected) $
              fail (name <> "." <> extension
                <> " differs; inspect the rendering before regenerating")
          _ -> fail "usage: diagram-test [--write directory]") outputs
  check "email-approval" (describe (EmailApproval.emailApproval EmailApproval.fakeSend))
  check "dino" (describe dinoProject)
  check "tictactoe" (describe game)
  check "review" (describe review)
  check "kitchen-sink" (describe kitchenSink)
  check "left-after-fanout" (describe leftAfterFanout)
  check "choice-with-bypasses" (describe choiceWithBypasses)
  check "downstream-fork" (describe downstreamFork)
  check "nested-choice" (describe nestedChoice)
  check "conversation" (describe conversation)
  check "split-loop" (describe splitLoop)
  check "nested-loop" (describe nestedLoop)
  check "parallel-loop" (describe parallelLoop)
  check "loop-in-choice" (describe loopInChoice)
  check "empty-loop" (describe emptyLoop)
  check "choice-in-repeat" (describe choiceInRepeat)

flatten :: [Item] -> [(Text,Maybe NodeKind)]
flatten = concatMap (\case
  ItemNode i kind _ -> [(i,Just kind)]
  ItemBox i _ children -> (i,Nothing):flatten children)
