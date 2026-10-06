{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The core, without GHC-only features: explicit codecs, ordinary combinators,
-- and a pure monad in place of IO. The same file runs under GHC (as the
-- agentic-portable-test suite) and under MicroHs (in CI).
module Main (main) where

import Agentic
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)

-- ---------------------------------------------------------------------------
-- Explicit codecs: a record, and a sum with payloads

data Joke = Joke Text Text
  deriving (Show, Eq)

instance Contract Joke where
  contract =
    record "A joke" $
      Joke
        <$> required "setup" "The setup line" (\(Joke s _) -> s)
        <*> required "punchline" "The line that lands it" (\(Joke _ p) -> p)

data Figure = Circle Double | Rect Double Double
  deriving (Show, Eq)

instance Contract Figure where
  contract =
    sumOf
      "A shape"
      [ constructor "Circle" "A circle" isCircle (Circle <$> required "radius" "" radius)
      , constructor "Rect" "A rectangle" isRect (Rect <$> required "width" "" width <*> required "height" "" height)
      ]
    where
      isCircle = \case Circle _ -> True; _ -> False
      isRect = \case Rect _ _ -> True; _ -> False
      radius = \case Circle r -> r; _ -> 0
      width = \case Rect w _ -> w; _ -> 0
      height = \case Rect _ h -> h; _ -> 0

-- ---------------------------------------------------------------------------
-- A pure monad: a script of model turns, and a log of what happened

data World = World {script :: [Action], logged :: [Text], events :: [Happened]}

newtype Pure a = Pure {runPure :: World -> (a, World)}

instance Functor Pure where
  fmap f (Pure g) = Pure (\w -> let (a, w') = g w in (f a, w'))

instance Applicative Pure where
  pure a = Pure (\w -> (a, w))
  Pure f <*> Pure g = Pure (\w -> let (h, w1) = f w; (a, w2) = g w1 in (h a, w2))

instance Monad Pure where
  Pure g >>= k = Pure (\w -> let (a, w1) = g w in (k a).runPure w1)

say :: Text -> Pure ()
say t = Pure (\w -> ((), w {logged = w.logged <> [t]}))

-- | The handlers: scripted turns, fixed judgements, and every event recorded.
handlers :: Runtime Pure
handlers =
  (runtimeWith (\e -> error ("flow error: " <> show e)))
    { systemTwo = SystemTwo $ \_ -> Pure $ \w -> case w.script of
        a : rest -> (Turn (Raw Null) a, w {script = rest})
        [] -> error "the script ran out of turns"
    , systemOne = SystemOne $ \request -> pure (map answer request.questions)
    , observe = \e -> Pure (\w -> ((), w {events = w.events <> [e.happened]}))
    }
  where
    answer = \case
      AskYesNo _ -> YesNoAnswer 0.9
      AskChoice _ ((l, _) : _) -> ChoiceAnswer l [(l, 1)] 1
      AskChoice _ [] -> ChoiceAnswer "" [] 0
      AskScore _ _ -> ScoreAnswer 1 [(1, 1)] 1

-- ---------------------------------------------------------------------------
-- The flows

countLetters :: Tool Pure
countLetters = tool @Text @Int "count_letters" "Count the letters in some text" $
  act (\t -> say ("count_letters ran on " <> t) >> pure (T.length t))

writeJoke :: Tool Pure
writeJoke = tool @Text @Joke "write_joke" "Write a joke about a topic" (draft @Joke "Write a joke about this topic")

jokeAndFigure :: Agentic Pure Text (Joke, Figure)
jokeAndFigure =
  draftWith @Joke [countLetters, writeJoke] "Write a joke, using the tools"
    &&& draft @Figure "Pick a shape"

review :: Agentic Pure Joke (YesNo, YesNo)
review = judge ((,) <$> yesNo "Is it funny?" <*> yesNo "Is it kind?")

joke :: Joke
joke = Joke "Why was the scarecrow promoted?" "He was outstanding in his field."

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "ok   " else "FAIL ") <> name)
        if ok then pure () else modifyIORef failures (+ 1)

  -- 1. Explicit codecs for a record and a payload-bearing sum.
  check "a record round-trips through its codec" (contract.decode (contract.encode joke) == Right joke)
  check "a sum with payloads round-trips" (contract.decode (contract.encode (Rect 2 3)) == Right (Rect 2 3))
  check "a sum encodes its constructor as a tag" (contract.encode (Circle 1) == Object [("tag", String "Circle"), ("radius", Number 1)])
  check "a record missing a field is rejected" (either (const True) (const False) ((contract @Joke).decode (Object [("setup", String "x")])))

  let world0 =
        World
          { script =
              [ CallTools [ToolCall "c1" "count_letters" (String "scarecrow")]
              , CallTools [ToolCall "c2" "write_joke" (String "farms")]
              , Respond (contract.encode joke) -- answers the nested write_joke draft
              , Respond (Object [("setup", String "only a setup")]) -- invalid: no punchline
              , Respond (contract.encode joke) -- the correction
              , Respond (contract.encode (Rect 2 3)) -- the shape
              ]
          , logged = []
          , events = []
          }
      ((result, verdict), world) =
        ((,) <$> interpret handlers jokeAndFigure "scarecrows" <*> interpret handlers review joke).runPure world0
      seen = world.events

  -- 2. A scripted tool call runs its typed body, and the draft continues.
  check "the tool's typed body ran with the model's input" (world.logged == ["count_letters ran on scarecrow"])
  check "the tool's typed result went back to the model" (any (\case ToolReturned "c1" (ToolOk (Integer 9)) -> True; _ -> False) seen)
  check "the draft continued to a typed response" (result == (joke, Rect 2 3))

  -- 3. A nested drafting tool.
  check "the nested tool ran its own draft" (length [() | Drafting _ <- seen] == 3)
  check "the nested draft's result went back as the tool's result" (any (\case ToolReturned "c2" (ToolOk v) -> contract.decode v == Right joke; _ -> False) seen)

  -- 4. Invalid output, then a corrected response.
  check "the invalid output was rejected" (length [() | OutputRejected _ <- seen] == 1)
  check "every scripted turn was used" (null world.script)

  -- 5. An applicative judgement batch: two questions, one request.
  check "two questions went in one request" ([length r.questions | Judged r _ <- seen] == [2])
  check "the answers decoded to typed values" (verdict == (YesNo 0.9, YesNo 0.9))

  -- 6. Describing the flow invokes no handlers: describe has no runtime to call.
  let tree = renderTree (describe jokeAndFigure)
  check "describe shows the drafts and both tools" (all (`T.isInfixOf` tree) ["draft @Joke", "tool count_letters  act", "tool write_joke  draft @Joke", "draft @Figure"])

  -- 7. All of the above ran in Pure, not IO.
  n <- readIORef failures
  if n == 0 then putStrLn "all checks passed" else putStrLn (show n <> " checks failed") >> exitFailure
