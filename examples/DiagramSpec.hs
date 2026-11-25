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
import Revision qualified
import Agentic.Scripted qualified as Scripted
import Data.IORef
import System.Environment (getArgs)

leftAfterFanout :: Agentic IO Int (Either (Int,Int) Int)
leftAfterFanout = (act pure `named` "source one" &&& act pure `named` "source two")
  >>> purely Left >>> left (act pure `named` "handle pair")

choiceWithBypasses :: Agentic IO (Either Int Int) (Int,Int)
choiceWithBypasses = (returnA &&& act pure `named` "x") ||| (returnA &&& act pure `named` "y")

downstreamFork :: Agentic IO (Either Int Int) (Int,Int)
downstreamFork = (returnA ||| returnA) >>> (act pure `named` "f" &&& act pure `named` "g")

nestedChoice :: Agentic IO (Either (Either Int Int) Int) Int
nestedChoice = (returnA ||| act pure `named` "inner") ||| returnA

choiceInRepeat :: Agentic IO Int Int
choiceInRepeat = repeatUntil (purely (>=3)) (purely Left >>> (purely (+1) ||| returnA))

-- The conversational reproduction from #19; describing it runs no model.
data Chat = Chat { outgoing :: Text, latestReply :: Text, finished :: Bool }

conversation :: Agentic IO () Text
conversation =
  (purely (const (Chat "Hello!" "" False)) `named` "Prepare greeting")
  >>> (repeatUntil (purely (.finished)) turn `named` "Chat until exit")
  >>> (purely (const "Finished") `named` "Finish conversation")
  where
    turn =
      ((purely id &&& (purely (.outgoing) >>> await @Text @Text))
        `named` "Ask a question and wait for reply")
      >>> (purely classify `named` "Is the reply exit?")
      >>> (stop ||| respond)
    classify (state, reply)
      | Text.toCaseFold (Text.strip reply) == "exit" = Left state
      | otherwise = Right (state { latestReply = reply })
    stop = purely (\state -> state { finished = True })
      `named` "Exit without calling the model"
    respond =
      (purely id &&& (purely (.latestReply) >>> answer))
      >>> (purely (\(state, reply) -> state { outgoing = reply })
        `named` "Remember answer for next turn")
    answer = draft @Text @Text "Reply briefly to the user's message."
      `named` "Generate answer"

splitLoop :: Agentic IO (Int,Int) (Int,Int)
splitLoop = repeatUntil (purely ((>=3) . fst)) (act pure *** act pure)
  >>> (act pure *** act pure)

nestedLoop :: Agentic IO Int Int
nestedLoop = repeatUntil (purely (>=3)) (repeatUntil (purely (>=2)) (act (pure . (+1))))

parallelLoop :: Agentic IO Int Int
parallelLoop = repeatUntil (purely (>=3))
  ((act pure &&& act pure) >>> purely (uncurry max))

loopInChoice :: Agentic IO (Either Int Int) Int
loopInChoice = repeatUntil (purely (>=3)) (act (pure . (+1))) ||| returnA

emptyLoop :: Agentic IO Int Int
emptyLoop = repeatUntil (purely (>=3)) returnA

-- Generalised tests exercise control flow inside the predicate.
forkTest :: Agentic IO Int Int
forkTest = repeatUntil
  ((act pure &&& act pure) >>> purely (\(a,b) -> a + b >= 6))
  (act (pure . (+1)))

choiceTest :: Agentic IO (Either Int Int) (Either Int Int)
choiceTest = repeatUntil (act (pure . (>=3)) ||| act (pure . (>=3))) returnA

splitTest :: Agentic IO (Int,Int) (Int,Int)
splitTest = repeatUntil
  ((act pure *** act pure) >>> purely (\(a,b) -> a + b >= 6)) returnA

nestedTest :: Agentic IO Int Int
nestedTest = repeatUntil
  (repeatUntil (purely (>=2)) (act (pure . (+1))) >>> purely (>=3))
  (act (pure . (+1)))

namedTest :: Agentic IO Int Int
namedTest = repeatUntil (purely (>=3) `named` "Finished?") returnA

checkRevision :: IO ()
checkRevision = do
  calls <- newIORef (0 :: Int)
  grades <- newIORef [0.2,0.9]
  let initial = Revision.Joke "Setup" "Old punchline"
      improved = Revision.Joke "Setup" "Better punchline"
      rt = runtime
        { systemOne = SystemOne (\request -> do
            remaining <- readIORef grades
            case remaining of
              confidence:rest -> do
                writeIORef grades rest
                let SystemOne answer = Scripted.fixedAnswers confidence
                answer request
              [] -> fail "unexpected extra review")
        , systemTwo = SystemTwo (\_ -> do
            modifyIORef' calls (+1)
            pure (Turn (Raw Null) (Scripted.respond improved)))
        }
  result <- interpret rt Revision.revise initial
  unless (result == improved) (fail "revision did not return the improved joke")
  readIORef calls >>= \n -> unless (n == 1) (fail "revision count differs")
  readIORef grades >>= \xs -> unless (null xs) (fail "final review was not run")

main :: IO ()
main = do
  args <- getArgs
  DinoSpec.check
  checkRevision
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
        let outputs = [("mmd",mermaid d),("dot",dot d)] <> [("tree",renderTree d) | name `elem` ["dino","tictactoe","revision"]]
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
  check "revision" (describe Revision.revise)
  check "fork-test" (describe forkTest)
  check "choice-test" (describe choiceTest)
  check "split-test" (describe splitTest)
  check "nested-test" (describe nestedTest)
  check "named-test" (describe namedTest)
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
