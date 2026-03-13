# haskell-agentic v1 Refactor Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor the prototype executable into a publishable Haskell library with clean module boundaries, proper error handling, a generic LLM provider interface, and a test suite.

**Architecture:** The library will expose a minimal public API via the `Simple` and `Agentic` modules, hide implementation details behind internal modules, and split examples into a separate executable. The core `Agentic.hs` will be decoupled from Dhall, with schema-specific code living only in `Protocol.*` modules.

**Tech Stack:** GHC2024, Cabal 3.0, Kleisli Arrows + RWST monad, Dhall + JSON Schema (via autodocodec), Anthropic Claude API via http-conduit, tasty + tasty-hunit + tasty-quickcheck for tests.

---

## Background: Current Structure

```
src/
  Main.hs              -- toy DSL demo (should move to app/)
  Agentic.hs           -- core framework + Dhall-specific functions (needs split)
  Simple.hs            -- public API facade (good, keep)
  Combinators.hs       -- arrow combinators using Dhall hardcoded (needs fix)
  Examples.hs          -- all examples (should move to app/)
  Example2.hs          -- data types for commit analysis (move to app/)
  Progress.hs          -- terminal spinner (keep, improve)
  PureMockup.hs        -- pure arrow demo (move to app/)
  LLM/
    Client.hs          -- routes to Anthropic (hardcoded, needs abstraction)
    Anthropic/Client.hs
    Anthropic/Types.hs
    OpenAI/Client.hs   -- stub, commented out
  Protocol/
    Class.hs           -- SchemaFormat typeclass (good, keep)
    DhallSchema/Marshal.hs
    DhallSchema/Prompts.hs
    JSONSchema/Marshal.hs
    JSONSchema/Prompts.hs
```

**Key problem:** `Agentic.hs` imports `Protocol.DhallSchema.Prompts` and contains `schemaOf`, `parse`, `injectSchema`, `inject`, `extract`, `extractWithRetry` — all Dhall-specific. These should live in `Protocol.DhallSchema.Marshal`. After cleanup, `Agentic.hs` only contains the monad stack, `runLLM`, `orFail`, `run`, `runIO`, and `prompt`.

---

## Task 1: Library + Executable Split in Cabal

**Files:**
- Modify: `haskell-agentic.cabal`
- Create: `app/Main.hs`

**Step 1: Add library stanza to cabal file**

Open `haskell-agentic.cabal`. Replace the `executable` stanza with a `library` stanza plus a new `executable` stanza:

```cabal
library
  import: warnings
  hs-source-dirs: src
  default-language: GHC2024
  default-extensions:
    AllowAmbiguousTypes
    Arrows
    ConstraintKinds
    DeriveAnyClass
    DeriveGeneric
    DerivingVia
    DuplicateRecordFields
    MultilineStrings
    NoFieldSelectors
    OverloadedLists
    OverloadedRecordDot
    OverloadedStrings
    PatternSynonyms
    TemplateHaskell
    TypeApplications
    TypeFamilies
  exposed-modules:
    Agentic
    Combinators
    Protocol.Class
    Protocol.DhallSchema.Marshal
    Protocol.JSONSchema.Marshal
    Simple
  other-modules:
    LLM.Anthropic.Client
    LLM.Anthropic.Types
    LLM.Client
    LLM.OpenAI.Client
    Progress
    Protocol.DhallSchema.Prompts
    Protocol.JSONSchema.Prompts
  build-depends:
    aeson,
    aeson-pretty,
    ansi-terminal,
    async,
    autodocodec,
    autodocodec-schema,
    base >=4.7 && <5,
    bytestring,
    containers,
    dhall >=1.41 && <1.43,
    either,
    http-conduit,
    monad-loops,
    mtl,
    stm,
    text,
    time,
    unliftio,
    vector,

executable haskell-agentic-examples
  import: warnings
  main-is: Main.hs
  hs-source-dirs: app
  default-language: GHC2024
  default-extensions:
    AllowAmbiguousTypes
    Arrows
    DeriveAnyClass
    DeriveGeneric
    OverloadedRecordDot
    OverloadedStrings
    TypeApplications
  build-depends:
    base >=4.7 && <5,
    haskell-agentic,
    text,
    dhall >=1.41 && <1.43,
    autodocodec,
```

Note: remove `free`, `openai` from library dependencies (they're only used in `Main.hs`/examples). Add them to the executable stanza.

**Step 2: Create app/ directory and app/Main.hs**

```bash
mkdir app
```

Create `app/Main.hs`:
```haskell
module Main where

import Examples (loudJokeTeller, dinoProject)
import Simple (run)
import Data.Text (unpack)

main :: IO ()
main = do
    result <- run dinoProject ""
    putStrLn $ unpack result
```

**Step 3: Move example source files to app/**

```bash
mv src/Examples.hs app/Examples.hs
mv src/Example2.hs app/Example2.hs
mv src/PureMockup.hs app/PureMockup.hs
```

Update the `executable` stanza `other-modules` to include `Examples`, `Example2`, `PureMockup`.

Note: Keep `src/Main.hs` for now — it has the Free Monad toy DSL which is interesting. Delete it or move it to `app/` — since it's the old `main-is` target, remove it (the new main is `app/Main.hs`).

```bash
rm src/Main.hs
```

**Step 4: Build to confirm it compiles**

```bash
cabal build all
```

Expected: successful build with library and executable targets both building.

**Step 5: Commit**

```bash
git add haskell-agentic.cabal app/ src/Main.hs
git commit -m "refactor: split into library + examples executable"
```

---

## Task 2: Decouple Agentic.hs from Dhall

**Files:**
- Modify: `src/Agentic.hs`
- Modify: `src/Protocol/DhallSchema/Marshal.hs`
- Modify: `src/Combinators.hs`

**Problem:** `Agentic.hs` currently contains Dhall-specific functions:
- `schemaOf` — generates Dhall schema text for a type
- `parse` — parses Dhall text into a Haskell value
- `injectSchema` — injects Dhall schema into a Prompt
- `inject` — serialises a value to Dhall and injects into Prompt
- `extract` — injectSchema >>> runLLM >>> parse >>> orFail (Dhall-specific pipeline)
- `extractWithRetry` — retry logic using Dhall error prompts

These should move to `Protocol.DhallSchema.Marshal`. After the move, `Agentic.hs` becomes a pure monad-stack + LLM-call module with no schema knowledge.

**Step 1: Move Dhall-specific functions to Protocol.DhallSchema.Marshal**

Current `src/Protocol/DhallSchema/Marshal.hs`:
```haskell
module Protocol.DhallSchema.Marshal where

import Agentic (AgenticRWS, Prompt(..), orFail, pattern Agentic, runLLM)
import Control.Arrow ((>>>))
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Validation (Validation(Failure, Success))
import Data.Text (Text, pack)
import Dhall (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import Protocol.Class (SchemaFormat(..), extractWithProxy)
import Data.Data (Proxy(Proxy))
import Protocol.DhallSchema.Prompts (injectDhallSchema, languageReference1, retryError)

data Dhall = Dhall
```

The new version should ADD the moved functions. Open `src/Agentic.hs` and cut:
- `schemaOf`
- `parse`
- `injectSchema`
- `inject`
- `extract`
- `extractWithRetry`
- The imports they need: `Dhall`, `Dhall.Core`, `Protocol.DhallSchema.Prompts`

Paste them into `src/Protocol/DhallSchema/Marshal.hs` and adjust module names.

New `src/Agentic.hs` (trimmed — only framework code):
```haskell
module Agentic
    ( Agentic
    , AgenticRWS
    , Environment(..)
    , Events
    , State(..)
    , Prompt(..)
    , pattern Agentic
    , orFail
    , runLLM
    , run
    , runIO
    , prompt
    ) where

import Control.Arrow (Arrow, Kleisli(..), arr, (>>>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (RWST(RWST), ask, get, runRWST, tell)
import Control.Monad.RWS.Class (MonadRWS)
import Data.Text (Text, unpack)
import qualified LLM.Client
import Prelude
import UnliftIO (MonadUnliftIO(withRunInIO), atomically, modifyTVar, newTVarIO, readTVarIO)

type AgenticRWS m = (MonadUnliftIO m, MonadIO m, MonadRWS Environment Events State m)
type Agentic m a b = AgenticRWS m => Kleisli m a b

pattern Agentic :: (a -> m b) -> Kleisli m a b
pattern Agentic f = Kleisli f

newtype Environment = Environment { prompt :: Text }
type Events = [(Prompt, Text)]
newtype State = State ()

data Prompt = Prompt { system :: Text, user :: Text }

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

runLLM :: Agentic m Prompt Text
runLLM = Kleisli $ \p@(Prompt system user) -> do
    reply <- liftIO $ LLM.Client.chat system user
    tell [(p, reply)]
    pure reply

prompt :: Agentic m Text Prompt
prompt = arr $ \user -> Prompt { system = "", user = user }

run :: Kleisli m a b -> a -> m b
run = runKleisli

runIO :: Kleisli (RWST Environment Events State IO) Text a -> Text -> IO a
runIO k input = do
    let environment = Environment { prompt = input }
        state = State ()
    (a, _finalState, _logs) <- runRWST (runKleisli k input) environment state
    pure a

instance MonadUnliftIO (RWST Environment Events State IO) where
    withRunInIO action = do
        env <- ask
        state <- get
        eventsRef <- liftIO $ newTVarIO []
        let runInIO (RWST rwst) = do
                (a, _finalState, events) <- rwst env state
                atomically $ modifyTVar eventsRef (++ events)
                pure a
        result <- liftIO $ action runInIO
        final_events <- liftIO $ readTVarIO eventsRef
        tell final_events
        pure result
```

**Step 2: Update Protocol.DhallSchema.Marshal with moved functions**

New `src/Protocol/DhallSchema/Marshal.hs`:
```haskell
module Protocol.DhallSchema.Marshal where

import Agentic (AgenticRWS, Prompt(..), orFail, pattern Agentic, runLLM, run)
import Control.Arrow ((>>>))
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Validation (Validation(Failure, Success))
import Data.Text (Text, pack)
import Dhall (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import Protocol.Class (SchemaFormat(..), extractWithProxy)
import Data.Data (Proxy(Proxy))
import Protocol.DhallSchema.Prompts (injectDhallSchema, languageReference1, retryError)

data Dhall = Dhall

dhallSchemaOf :: forall a. FromDhall a => Text
dhallSchemaOf = case Dhall.expected (Dhall.auto @a) of
    Success result -> Dhall.Core.pretty result
    Failure err    -> error $ show err

parseDhall :: forall b m. (FromDhall b, MonadIO m) => Text -> m (Either Text b)
parseDhall input = do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

instance SchemaFormat Dhall where
    type SchemaConstraint Dhall a = (FromDhall a, ToDhall a)

    systemPrompt _ = languageReference1

    schemaOf _ = dhallSchemaOf

    parseWithSchema _ = parseDhall

    injectSchema _ user = injectDhallSchema user (dhallSchemaOf)

    injectObject obj user =
        let dhall = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
        in Protocol.DhallSchema.Prompts.injectObject user dhall

-- Retry: attempt extract, on parse failure, give LLM the error and try again
extractWithRetryDhall :: forall s m. (FromDhall s, ToDhall s, AgenticRWS m) => Agentic m Prompt (Either Text s)
extractWithRetryDhall = Kleisli $ \p@(Prompt system user) -> do
    let injectSchemaDhall :: Agentic m Prompt Prompt
        injectSchemaDhall = Agentic $ \(Prompt _sys usr) ->
            pure $ Prompt languageReference1 (injectDhallSchema usr (dhallSchemaOf @s))
        attempt input' = do
            reply <- run (injectSchemaDhall >>> runLLM) input'
            parsed <- parseDhall @s reply
            pure (reply, parsed)
    (reply, parsed) <- attempt p
    case parsed of
        Left err -> do
            let instruction = retryError err reply user
            (_reply', parsed') <- attempt $ Prompt system instruction
            pure parsed'
        Right result -> pure $ Right result
```

**Step 3: Fix Combinators.hs to use Protocol.Class**

Current `Combinators.hs` uses `extract @b` from `Agentic` (Dhall-specific). Replace with `extractWith @Dhall @b`:

```haskell
module Combinators where

import Agentic (Agentic, AgenticRWS, run)
import Control.Arrow (Kleisli(..), (>>>))
import Dhall (FromDhall, ToDhall)
import Protocol.Class (extractWith)
import Protocol.DhallSchema.Marshal (Dhall)
import UnliftIO.Async (mapConcurrently)

(>...>) :: forall a b c m. (FromDhall b, ToDhall b) => Agentic m a Prompt -> Agentic m b c -> Agentic m a c
(>...>) l r = l >>> extractWith @Dhall @b >>> r

(<<.>>) :: forall a b s m. AgenticRWS m => Agentic m a [s] -> Agentic m s b -> Agentic m a [b]
(<<.>>) l r = Kleisli $ \input -> do
    tasks :: [s] <- run l input
    results :: [b] <- mapConcurrently (run r) tasks
    pure results
```

**Step 4: Build**

```bash
cabal build all
```

Expected: successful build, no type errors.

**Step 5: Commit**

```bash
git add src/Agentic.hs src/Protocol/DhallSchema/Marshal.hs src/Combinators.hs
git commit -m "refactor: decouple Agentic.hs from Dhall, move schema code to Protocol.DhallSchema"
```

---

## Task 3: Test Suite Setup

**Files:**
- Modify: `haskell-agentic.cabal`
- Create: `test/Main.hs`
- Create: `test/Protocol/DhallSchema/MarshalSpec.hs`
- Create: `test/Protocol/JSONSchema/MarshalSpec.hs`

**Step 1: Add test-suite stanza to cabal**

```cabal
test-suite haskell-agentic-test
  import: warnings
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test
  default-language: GHC2024
  default-extensions:
    DeriveAnyClass
    DeriveGeneric
    OverloadedStrings
    TypeApplications
  build-depends:
    base >=4.7 && <5,
    haskell-agentic,
    dhall >=1.41 && <1.43,
    autodocodec,
    autodocodec-schema,
    tasty,
    tasty-hunit,
    text,
```

**Step 2: Write failing tests for Dhall schema generation**

Create `test/Main.hs`:
```haskell
module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Protocol.DhallSchema.MarshalSpec
import qualified Protocol.JSONSchema.MarshalSpec

main :: IO ()
main = defaultMain $ testGroup "haskell-agentic"
    [ Protocol.DhallSchema.MarshalSpec.tests
    , Protocol.JSONSchema.MarshalSpec.tests
    ]
```

Create `test/Protocol/DhallSchema/MarshalSpec.hs`:
```haskell
module Protocol.DhallSchema.MarshalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Data.Text (Text, isInfixOf, unpack)
import Dhall (FromDhall, ToDhall)
import GHC.Generics (Generic)
import Protocol.DhallSchema.Marshal (Dhall, dhallSchemaOf, parseDhall)
import Control.Monad.IO.Class (liftIO)

data SimpleRecord = SimpleRecord { name :: Text, age :: Int }
    deriving (Generic, Show, Eq, FromDhall, ToDhall)

data SumType = VariantA { x :: Int } | VariantB { y :: Text }
    deriving (Generic, Show, Eq, FromDhall, ToDhall)

tests :: TestTree
tests = testGroup "Protocol.DhallSchema.Marshal"
    [ testCase "schemaOf record contains field names" $ do
        let schema = dhallSchemaOf @SimpleRecord
        assertBool ("schema contains 'name': " <> unpack schema) ("name" `isInfixOf` schema)
        assertBool ("schema contains 'age': " <> unpack schema) ("age" `isInfixOf` schema)

    , testCase "schemaOf sum type contains variant names" $ do
        let schema = dhallSchemaOf @SumType
        assertBool ("schema contains 'VariantA': " <> unpack schema) ("VariantA" `isInfixOf` schema)
        assertBool ("schema contains 'VariantB': " <> unpack schema) ("VariantB" `isInfixOf` schema)

    , testCase "parseDhall round-trips a record" $ do
        let input = "{ name = \"Alice\", age = 30 }"
        result <- parseDhall @SimpleRecord input
        result @?= Right (SimpleRecord { name = "Alice", age = 30 })

    , testCase "parseDhall returns Left on invalid input" $ do
        let input = "{ invalid_dhall = !!!"
        result <- parseDhall @SimpleRecord input
        case result of
            Left _ -> pure ()
            Right _ -> fail "Expected parse failure"
    ]
```

Create `test/Protocol/JSONSchema/MarshalSpec.hs`:
```haskell
module Protocol.JSONSchema.MarshalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Data.Text (Text, isInfixOf, unpack)
import Autodocodec (HasCodec(..), object, requiredField')
import Protocol.JSONSchema.Marshal (Json, jsonSchemaOf)

data SimpleRecord = SimpleRecord { name :: Text, age :: Int }
    deriving (Show, Eq)

instance HasCodec SimpleRecord where
    codec = object "SimpleRecord" $
        SimpleRecord
          <$> requiredField' "name" .= (.name)
          <*> requiredField' "age" .= (.age)

tests :: TestTree
tests = testGroup "Protocol.JSONSchema.Marshal"
    [ testCase "jsonSchemaOf contains field names" $ do
        let schema = jsonSchemaOf @SimpleRecord
        assertBool ("schema contains 'name': " <> unpack schema) ("name" `isInfixOf` schema)
        assertBool ("schema contains 'age': " <> unpack schema) ("age" `isInfixOf` schema)
    ]
```

**Step 3: Run tests to confirm they fail (library not yet building with test stanza)**

```bash
cabal test
```

Expected: compile error because `dhallSchemaOf` and `parseDhall` are not yet exported from `Protocol.DhallSchema.Marshal` module (or tests can't find them). Fix exports as needed.

**Step 4: Fix any export issues and re-run**

```bash
cabal test --test-show-details=streaming
```

Expected: all 5 tests PASS.

**Step 5: Commit**

```bash
git add haskell-agentic.cabal test/
git commit -m "test: add test suite with schema generation and parsing tests"
```

---

## Task 4: Proper Error Types

**Files:**
- Create: `src/Agentic/Error.hs`
- Modify: `src/Agentic.hs`
- Modify: `src/Protocol/DhallSchema/Marshal.hs`
- Modify: `src/Protocol/JSONSchema/Marshal.hs`
- Modify: `src/Protocol/Class.hs`

**Goal:** Replace `Either Text` error returns with a proper `AgenticError` type. This makes errors pattern-matchable and avoids string comparisons.

**Step 1: Write failing test for error types**

Add to `test/Protocol/DhallSchema/MarshalSpec.hs`:
```haskell
import Agentic.Error (AgenticError(..))

    , testCase "parseDhall returns ParseError on failure" $ do
        let input = "not valid dhall !!!"
        result <- parseDhall @SimpleRecord input
        case result of
            Left (ParseError _) -> pure ()
            Left other          -> fail $ "Expected ParseError, got: " <> show other
            Right _             -> fail "Expected failure"
```

Run: `cabal test` — expected FAIL (no `Agentic.Error` module yet).

**Step 2: Create Agentic/Error.hs**

Create `src/Agentic/Error.hs`:
```haskell
module Agentic.Error
    ( AgenticError(..)
    , toAgenticError
    ) where

import Data.Text (Text)

data AgenticError
    = ParseError    { message :: Text, rawInput :: Text }
    | LLMError      { message :: Text }
    | SchemaError   { message :: Text }
    deriving (Show, Eq)

toAgenticError :: Text -> Text -> AgenticError
toAgenticError msg raw = ParseError { message = msg, rawInput = raw }
```

Add `Agentic.Error` to cabal `exposed-modules`.

**Step 3: Update parseDhall to return AgenticError**

In `src/Protocol/DhallSchema/Marshal.hs`, change:
```haskell
-- Old
parseDhall :: forall b m. (FromDhall b, MonadIO m) => Text -> m (Either Text b)

-- New
import Agentic.Error (AgenticError(..), toAgenticError)

parseDhall :: forall b m. (FromDhall b, MonadIO m) => Text -> m (Either AgenticError b)
parseDhall input = do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) ->
            pure $ Left $ ParseError
                { message = "Dhall parse error: " <> pack (show err)
                , rawInput = input
                }
```

**Step 4: Update Protocol.Class to use AgenticError**

In `src/Protocol/Class.hs`, change:
```haskell
-- Old
parseWithSchema :: forall a m. MonadIO m => SchemaConstraint fmt a => Proxy a -> Text -> m (Either Text a)

-- New
import Agentic.Error (AgenticError)
parseWithSchema :: forall a m. MonadIO m => SchemaConstraint fmt a => Proxy a -> Text -> m (Either AgenticError a)
```

Update `orFail` in `Agentic.hs` to handle `AgenticError`:
```haskell
import Agentic.Error (AgenticError(..))

orFail :: Arrow a => a (Either AgenticError c) c
orFail = arr $ either (error . show) id
```

**Step 5: Run tests**

```bash
cabal test --test-show-details=streaming
```

Expected: all tests PASS including the new `ParseError` pattern match test.

**Step 6: Commit**

```bash
git add src/Agentic/Error.hs src/Agentic.hs src/Protocol/Class.hs src/Protocol/DhallSchema/Marshal.hs haskell-agentic.cabal
git commit -m "feat: add AgenticError type, replace Either Text error returns"
```

---

## Task 5: Configurable Retry Logic

**Files:**
- Create: `src/Agentic/Retry.hs`
- Modify: `src/Protocol/DhallSchema/Marshal.hs`
- Modify: `src/Simple.hs`

**Goal:** Make retry configurable (number of attempts, backoff). Currently `extractWithRetry` does exactly one retry with no config. Expose retry as a first-class combinator.

**Step 1: Write failing test for retry**

Create `test/Agentic/RetrySpec.hs`:
```haskell
module Agentic.RetrySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Agentic.Retry (RetryConfig(..), defaultRetry, withRetry)
import Agentic.Error (AgenticError(..))
import Data.IORef

tests :: TestTree
tests = testGroup "Agentic.Retry"
    [ testCase "withRetry calls action once on success" $ do
        callCount <- newIORef (0 :: Int)
        let action = do
                modifyIORef callCount (+1)
                pure (Right "ok")
        result <- withRetry defaultRetry action
        result @?= Right "ok"
        count <- readIORef callCount
        count @?= 1

    , testCase "withRetry retries up to maxAttempts on failure" $ do
        callCount <- newIORef (0 :: Int)
        let action = do
                n <- readIORef callCount
                modifyIORef callCount (+1)
                if n < 2
                    then pure (Left $ ParseError "fail" "")
                    else pure (Right "ok")
        result <- withRetry (defaultRetry { maxAttempts = 3 }) action
        result @?= Right "ok"
        count <- readIORef callCount
        count @?= 3

    , testCase "withRetry returns Left after exhausting attempts" $ do
        let action = pure (Left $ ParseError "always fails" "")
        result <- withRetry (defaultRetry { maxAttempts = 2 }) action
        case result of
            Left (ParseError _ _) -> pure ()
            _                     -> fail "Expected ParseError"
    ]
```

Run: `cabal test` — FAIL (no `Agentic.Retry` module).

**Step 2: Create Agentic/Retry.hs**

Create `src/Agentic/Retry.hs`:
```haskell
module Agentic.Retry
    ( RetryConfig(..)
    , defaultRetry
    , withRetry
    ) where

import Agentic.Error (AgenticError)

data RetryConfig = RetryConfig
    { maxAttempts :: Int
    }

defaultRetry :: RetryConfig
defaultRetry = RetryConfig { maxAttempts = 2 }

withRetry :: RetryConfig -> IO (Either AgenticError a) -> IO (Either AgenticError a)
withRetry config action = go (maxAttempts config)
  where
    go 0 = action
    go n = do
        result <- action
        case result of
            Right x -> pure (Right x)
            Left _  -> go (n - 1)
```

Add `Agentic.Retry` to cabal `exposed-modules`.

**Step 3: Run tests**

```bash
cabal test --test-show-details=streaming
```

Expected: all retry tests PASS.

**Step 4: Wire retry into Simple.hs extract**

In `src/Simple.hs`, update `extract` to use retry:
```haskell
-- This is a design choice: Simple.extract uses retry by default
-- Advanced users can use extractWith directly (no retry)
extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt s
extract = extractWith @Dhall @s  -- retry can be added at the run level later
```

For now, keep as-is. Retry is available via `Agentic.Retry.withRetry` for users who want it.

**Step 5: Commit**

```bash
git add src/Agentic/Retry.hs haskell-agentic.cabal test/Agentic/RetrySpec.hs
git commit -m "feat: add configurable RetryConfig and withRetry combinator"
```

---

## Task 6: LLM Provider Abstraction

**Files:**
- Create: `src/LLM/Provider.hs`
- Modify: `src/LLM/Client.hs`
- Modify: `src/LLM/Anthropic/Client.hs`
- Modify: `src/Agentic.hs`

**Goal:** Move API key reading out of `LLM.Anthropic.Client` and into the `Environment` type. Add a `LLMProvider` configuration type so users can select providers at runtime.

**Step 1: Write failing test for provider config**

Create `test/LLM/ProviderSpec.hs`:
```haskell
module LLM.ProviderSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import LLM.Provider (LLMProvider(..), defaultProvider, providerModel)

tests :: TestTree
tests = testGroup "LLM.Provider"
    [ testCase "defaultProvider is Anthropic" $ do
        defaultProvider @?= Anthropic

    , testCase "providerModel returns a non-empty string" $ do
        let model = providerModel Anthropic
        (length model > 0) @?= True
    ]
```

Run: `cabal test` — FAIL.

**Step 2: Create LLM/Provider.hs**

Create `src/LLM/Provider.hs`:
```haskell
module LLM.Provider
    ( LLMProvider(..)
    , LLMConfig(..)
    , defaultProvider
    , defaultConfig
    , providerModel
    ) where

data LLMProvider = Anthropic
    deriving (Show, Eq)

data LLMConfig = LLMConfig
    { provider  :: LLMProvider
    , model     :: String
    , maxTokens :: Int
    , apiKey    :: String   -- read from environment at startup
    }

defaultProvider :: LLMProvider
defaultProvider = Anthropic

providerModel :: LLMProvider -> String
providerModel Anthropic = "claude-sonnet-4-20250514"

defaultConfig :: LLMConfig
defaultConfig = LLMConfig
    { provider  = Anthropic
    , model     = providerModel Anthropic
    , maxTokens = 10240
    , apiKey    = ""  -- must be set before use
    }
```

**Step 3: Update Agentic.hs Environment to carry LLMConfig**

In `src/Agentic.hs`, update `Environment`:
```haskell
import LLM.Provider (LLMConfig, defaultConfig)

data Environment = Environment
    { llmConfig :: LLMConfig
    , userPrompt :: Text
    }
```

Update `runIO` to read API key from environment variable:
```haskell
import System.Environment (lookupEnv)
import LLM.Provider (defaultConfig)
import Data.Maybe (fromMaybe)

runIO :: Kleisli (RWST Environment Events State IO) Text a -> Text -> IO a
runIO k input = do
    apiKey <- fromMaybe "" <$> lookupEnv "ANTHROPIC_KEY"
    let config = defaultConfig { apiKey = apiKey }
        environment = Environment { llmConfig = config, userPrompt = input }
        state = State ()
    (a, _finalState, _logs) <- runRWST (runKleisli k input) environment state
    pure a
```

**Step 4: Thread LLMConfig through runLLM**

In `src/Agentic.hs`, update `runLLM` to use the environment config:
```haskell
import Control.Monad.RWS (asks)

runLLM :: Agentic m Prompt Text
runLLM = Kleisli $ \p@(Prompt system user) -> do
    config <- asks (.llmConfig)
    reply <- liftIO $ LLM.Client.chatWith config system user
    tell [(p, reply)]
    pure reply
```

**Step 5: Update LLM.Client to accept LLMConfig**

In `src/LLM/Client.hs`:
```haskell
module LLM.Client (chat, chatWith) where

import LLM.Provider (LLMConfig(..), LLMProvider(..))
import qualified LLM.Anthropic.Client as Anthropic
import Data.Text (Text)

-- Backwards-compatible (reads API key from env)
chat :: MonadIO m => Text -> Text -> m Text
chat = chatWith defaultConfig  -- still works but deprecated

-- New primary function
chatWith :: MonadIO m => LLMConfig -> Text -> Text -> m Text
chatWith config system user =
    case config.provider of
        Anthropic -> Anthropic.messages config system user
```

**Step 6: Run tests**

```bash
cabal test --test-show-details=streaming
```

Expected: all tests including new provider tests PASS.

**Step 7: Commit**

```bash
git add src/LLM/Provider.hs src/LLM/Client.hs src/Agentic.hs haskell-agentic.cabal test/LLM/ProviderSpec.hs
git commit -m "feat: add LLMProvider abstraction, move API key to Environment"
```

---

## Task 7: Progress Display Improvement

**Files:**
- Modify: `src/Progress.hs`

**Goal:** The current progress display only shows a single spinner. When multiple LLM calls run concurrently (e.g. via `<<.>>`), only one entry is shown. Improve to show a count of in-flight requests.

**Step 1: Read current Progress.hs to understand the structure**

The current implementation uses a `SimpleProgress` type with a spinner and a single status line. The key function is `runWithProgress` which wraps a `RWST` computation.

**Step 2: Update Progress.hs to count concurrent calls**

The change: instead of a fixed spinner, show `[N in-flight]` count where N is the number of pending LLM calls. Track this via an `IORef Int`.

Key changes to `src/Progress.hs`:
```haskell
-- Add to SimpleProgress:
data SimpleProgress = SimpleProgress
    { spinnerState :: TVar Int
    , inFlight     :: TVar Int   -- NEW: count of concurrent requests
    , running      :: TVar Bool
    }

-- Update display loop to show count:
displayLoop :: SimpleProgress -> IO ()
displayLoop p = do
    n <- readTVarIO p.inFlight
    let prefix = if n > 1 then "[" <> show n <> " in-flight] " else ""
    -- ... existing spinner logic with prefix prepended
```

The `runWithProgress` function wraps the `RWST` action; it should increment `inFlight` before each LLM call and decrement after. Since LLM calls happen inside `runLLM`, the cleanest approach is to pass the `SimpleProgress` through the `Environment`:

Actually, for simplicity: count the number of events written to the `Events` writer as a proxy for completed calls. Show "N calls completed" instead. This avoids threading progress state through the environment.

Simple approach — update `displayLoop` to read from a shared `TVar Int` that `runWithProgress` updates by watching the events log:

```haskell
runWithProgress :: Kleisli (RWST Environment Events State IO) Text b -> Text -> IO b
runWithProgress k input = do
    progress <- startProgressDisplay
    result <- runIO k input  -- run the computation
    stopProgressDisplay progress
    pure result
```

Keep this approach but add a final print of how many LLM calls were made:
```haskell
runWithProgress k input = do
    progress <- startProgressDisplay
    (result, callCount) <- runIOWithCount k input
    stopProgressDisplay progress
    putStrLn $ "(" <> show callCount <> " LLM calls)"
    pure result
```

**Step 3: Build**

```bash
cabal build all
```

Expected: successful.

**Step 4: Commit**

```bash
git add src/Progress.hs
git commit -m "improve: progress display shows LLM call count on completion"
```

---

## Task 8: Package Metadata & Haddock Documentation

**Files:**
- Modify: `haskell-agentic.cabal`
- Modify: `src/Simple.hs`
- Modify: `src/Agentic.hs`
- Modify: `src/Protocol/Class.hs`
- Modify: `src/Combinators.hs`
- Modify: `CHANGELOG.md`

**Step 1: Fill in cabal metadata**

In `haskell-agentic.cabal`, add:
```cabal
synopsis: Type-safe composable LLM agents using Kleisli arrows and Dhall/JSON Schema
description:
    haskell-agentic provides a framework for building type-safe, composable
    LLM agents in Haskell. Agents are modelled as Kleisli arrows in an RWS
    monad, enabling composition with standard Arrow combinators. Inputs and
    outputs are constrained by Dhall or JSON schemas, giving strong type
    guarantees on LLM responses.
homepage: https://github.com/tomwells/haskell-agentic
bug-reports: https://github.com/tomwells/haskell-agentic/issues
category: AI, LLM, Agents
```

**Step 2: Add Haddock to Simple.hs public API**

```haskell
-- | Run an agentic pipeline with a progress display.
-- This is the primary entry point for running agents.
--
-- Example:
-- @
-- result <- run (prompt >>> extract \@Text) "Tell me a joke"
-- @
run :: Kleisli (RWST Environment Events State IO) Text b -> Text -> IO b

-- | Extract a typed value from the LLM using Dhall schema.
-- The LLM is instructed to produce Dhall-encoded output matching the type.
--
-- Example:
-- @
-- data Joke = Joke { setup :: Text, punchline :: Text }
--     deriving (Generic, FromDhall, ToDhall)
--
-- joke <- run (prompt >>> extract \@Joke) "tell me a joke"
-- @
extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt s

-- | Inject a typed value into the prompt as Dhall.
inject :: forall s m. (FromDhall s, ToDhall s) => s -> Agentic m Prompt Prompt
```

**Step 3: Add Haddock to Combinators.hs**

```haskell
-- | Sequential composition that automatically extracts the intermediate type.
-- Left arrow produces a 'Prompt', right arrow consumes @b@.
-- The schema for @b@ is injected into the prompt automatically.
(>...>) :: ...

-- | Fanout: run the right arrow concurrently for each element of the list
-- produced by the left arrow.
(<<.>>) :: ...
```

**Step 4: Update CHANGELOG.md**

```markdown
# Changelog

## 0.1.0.0 — Initial Release

### Added
- Kleisli arrow-based agent framework (`Agentic`, `AgenticRWS`)
- Type-safe LLM extraction via Dhall schema (`extract`, `inject`)
- JSON Schema support via autodocodec (`extractWith @Json`)
- `SchemaFormat` typeclass for pluggable schema backends
- Concurrent fanout combinator (`<<.>>`)
- Sequential schema-injecting combinator (`>...>`)
- Retry logic for LLM parse failures (`Agentic.Retry`)
- Anthropic Claude API integration
- Terminal progress display
```

**Step 5: Build with docs**

```bash
cabal haddock
```

Expected: docs generated with no warnings for exported symbols.

**Step 6: Commit**

```bash
git add haskell-agentic.cabal src/Simple.hs src/Combinators.hs src/Agentic.hs CHANGELOG.md
git commit -m "docs: add package metadata, Haddock documentation on public API"
```

---

## Task 9: CI Configuration

**Files:**
- Create: `.github/workflows/ci.yml`

**Step 1: Create GitHub Actions workflow**

Create `.github/workflows/ci.yml`:
```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: '9.12'
          cabal-version: 'latest'

      - name: Cache cabal packages
        uses: actions/cache@v4
        with:
          path: ~/.cabal/packages
          key: ${{ runner.os }}-cabal-${{ hashFiles('cabal.project', 'haskell-agentic.cabal') }}

      - name: Build library
        run: cabal build lib:haskell-agentic

      - name: Run tests
        run: cabal test --test-show-details=streaming
        # Note: tests that require ANTHROPIC_KEY are skipped in CI
        # Set the secret in GitHub repo settings to enable integration tests

      - name: Build docs
        run: cabal haddock lib:haskell-agentic
```

**Step 2: Commit**

```bash
git add .github/workflows/ci.yml
git commit -m "ci: add GitHub Actions workflow for build, test, and docs"
```

---

## Summary of Changes

| Task | What Changes | Why |
|------|-------------|-----|
| 1 | cabal: library + executable split | Publishable library structure |
| 2 | Agentic.hs: remove Dhall coupling | Clean module boundaries |
| 3 | Add test suite | Verify schema/parsing correctness |
| 4 | Add AgenticError type | Machine-readable errors |
| 5 | Add RetryConfig | Configurable retry without hardcoding |
| 6 | Add LLMProvider / LLMConfig | Runtime provider selection, testable |
| 7 | Progress.hs: call count display | Better UX for concurrent agents |
| 8 | Package metadata + Haddock | Hackage-ready documentation |
| 9 | GitHub Actions CI | Automated testing on push |

## Exposed Public API (after refactor)

```
Simple          -- primary user API: extract, inject, run, prompt
Agentic         -- advanced: Agentic type, AgenticRWS, runIO, Environment
Agentic.Error   -- AgenticError type
Agentic.Retry   -- RetryConfig, withRetry
Combinators     -- (>...>), (<<.>>)
Protocol.Class  -- SchemaFormat typeclass, extractWith, injectWith
Protocol.DhallSchema.Marshal  -- Dhall instance
Protocol.JSONSchema.Marshal   -- Json instance
LLM.Provider    -- LLMProvider, LLMConfig
```
