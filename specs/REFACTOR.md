# haskell-agentic v1 Refactor Specification

## Context

This document guides the refactor of `haskell-agentic` from its current prototype state to a v1 library release. The prototype proved the core ideas work — type-safe structured LLM output via Dhall and JSON Schema, composable agents via Kleisli arrows, and agentic loops with typed state. This refactor preserves those wins while fixing the architectural problems that prevent library release.

The guiding philosophy: **the library should practise what it preaches**. The talk that accompanies this library argues that effect systems encode architecture and make boundaries machine-readable. The library's own architecture should demonstrate this.

---

## 1. Effect System Foundation

### Decision: Use `effectful` (via `effectful-core`)

Replace the monolithic `AgenticRWS` constraint with discrete effects using the `effectful` library.

**Why effectful:**
- O(1) effect dispatch (IORef-backed, not free monad interpretation chains)
- Constraint-based style (`Effect :> es`) composes naturally and reads as a capability list
- Good GHC error messages
- Active maintenance, ecosystem momentum (polysemy is stale, bluefin is too niche)

**Why `effectful-core` specifically:**
- Ships the `Eff` monad, dispatch machinery, `:>` constraint, `Effect` kind, and the pure effects (State, Reader, Writer, Error)
- Does NOT include IO-based effects (filesystem, concurrency, processes) — those live in the full `effectful` package
- The core library's schema derivation and agent loop logic should not depend on the full `effectful` package; only the provider interpreter packages need real IO

### Migration from RWS

The current design:

```haskell
type AgenticRWS m = (MonadIO m, MonadRWS Environment Events State m)
```

Replace with discrete effects:

```haskell
-- Configuration: read-only environment for the agent run
data AgentConfig :: Effect where ...

-- Observability: structured event emission
data AgentEvents :: Effect where ...

-- Session: per-agent mutable session state
data AgentSession :: Effect where ...

-- The LLM capability (see Section 2)
data LLM :: Effect where ...
```

This solves the concurrency problem with State. The current `MonadRWS` gives you a single shared mutable State across parallel branches (the dinosaur fanout example). With discrete effects, session state can be scoped per-agent or use `Concurrent` state where needed, and the interpreters control the concurrency semantics.

---

## 2. LLM as an Effect

### The Core Change

Extract the LLM call out of the library core entirely. Define it as a provider-agnostic effect.

### Core LLM Effect (provider-agnostic)

```haskell
data LLM :: Effect where
  Call :: LLMRequest -> LLM m LLMResponse
```

The request/response types cover what agents semantically need, independent of provider:

```haskell
data LLMRequest = LLMRequest
  { messages     :: [Message]
  , tools        :: [ToolSchema]
  , schema       :: Maybe SchemaFormat  -- Dhall or JSON Schema constraint
  , systemPrompt :: Maybe Text
  , ...
  }

data LLMResponse = LLMResponse
  { content   :: Maybe Text
  , toolCalls :: [ToolCall]
  , usage     :: Maybe Usage
  , ...
  }
```

These types represent the semantic level — "messages, tools, schema constraint" — not wire format. The interpreter handles translation to provider-specific wire formats.

### Provider Interpreters

Each provider interprets the `LLM` effect and closes over its own config:

```haskell
runAnthropic :: AnthropicConfig -> Eff (LLM : es) a -> Eff es a
runOpenAI    :: OpenAIConfig    -> Eff (LLM : es) a -> Eff es a
runOllama    :: OllamaConfig    -> Eff (LLM : es) a -> Eff es a
```

**Key design rules:**
- Agent code only ever sees `LLM :> es`. Never provider config.
- Provider config (credentials, model, base URL) is closed over by the interpreter.
- The core `haskell-agentic` package has ZERO http dependencies. Only the provider packages depend on `http-client`, `aeson`, etc.
- Wire format differences (tool call encoding, streaming deltas, system message placement) are absorbed by the interpreter.

**Future: Provider-specific extensions.** Frontier LLM features (Anthropic extended thinking, OpenAI reasoning effort, etc.) will eventually need provider-specific extension effects that compose alongside the core `LLM` effect. The architecture supports this naturally — an `AnthropicExt` effect could be added later, and agents that use it would declare it in their type signature, making the provider coupling visible and compiler-enforced. This is deferred to post-v1.

### Composition at the Edge

The caller's `main` is the only place that knows which provider is in play:

```haskell
main :: IO ()
main = do
  config <- loadAnthropicConfig
  result <- runEff
    . runAnthropic config
    . runAgentConfig myConfig
    . runAgentEvents printEvents
    $ myAgent "input"
```

Swapping providers is changing one line. Load-balancing or fallback across providers is a custom interpreter — the agent logic doesn't change.

---

## 3. Package Structure

Single package for now, with module namespaces that reflect the logical boundaries. This can be split into separate packages later once the boundaries are proven.

```
haskell-agentic/
├── src/
│   ├── Agentic.hs                    # Re-export public API
│   ├── Agentic/
│   │   ├── Core.hs                   # Agent type, composition primitives
│   │   ├── Effects.hs                # LLM effect, AgentConfig, AgentEvents, AgentSession
│   │   ├── Schema.hs                 # SchemaFormat GADT, extract, extractWith, inject
│   │   ├── Schema/
│   │   │   ├── Dhall.hs              # Dhall schema generation and parsing
│   │   │   └── Json.hs               # JSON Schema generation and parsing
│   │   ├── Tools.hs                  # Tool GADT, mkTool, dispatch
│   │   ├── Prompt.hs                 # prompt, inject, extract primitives
│   │   ├── Retry.hs                  # Internal retry logic for extract
│   │   ├── Error.hs                  # SchemaError, LLMError, ToolError types
│   │   └── Provider/
│   │       ├── Anthropic.hs          # runAnthropic interpreter
│   │       └── OpenAI.hs             # runOpenAI interpreter
├── examples/
│   ├── TicTacToe.hs
│   ├── DinoProject.hs
│   ├── JokeTelling.hs
│   └── ...
└── haskell-agentic.cabal
```

The `Agentic.Provider.*` namespace is where a future package split would happen — those modules are the ones that pull in `http-client` and provider-specific dependencies. For now they live in the same package, but the module boundary is clean enough that extraction into `haskell-agentic-anthropic` / `haskell-agentic-openai` is straightforward later.

---

## 4. Agent Composition Model

### Plain Functions as Core

The prototype uses Kleisli arrows, but the bodies are all monadic — almost every definition is `Agentic $ \input -> do ...`. The Arrow abstraction isn't earning its keep as the foundation.

**Decision:** The core agent type is a plain effectful function:

```haskell
type Agent es a b = a -> Eff es b
```

Sequential composition is `(>=>)` or plain function composition. Parallel fanout is a library combinator:

```haskell
-- Map an agent over a list (the <<.>> equivalent)
fanoutMap :: Agent es a b -> Agent es [a] [b]

-- Run agents in parallel on the same input
parallel :: Agent es a b -> Agent es a c -> Agent es a (b, c)
```

The `es` parameter is where the effects live. An agent's type signature declares its capabilities:

```haskell
researchDino :: (LLM :> es, Logging :> es) => Agent es Text Dino
```

### Arrow Instance as Optional Wrapper (post-v1)

The `Arrow` typeclass can be implemented over `Agent es` later as an optional layer. Since `Agent es a b` is just `Kleisli (Eff es) a b` in disguise, a newtype wrapper is straightforward:

```haskell
newtype ArrowAgent es a b = ArrowAgent { runArrowAgent :: Agent es a b }

instance Arrow (ArrowAgent es) where ...
instance ArrowChoice (ArrowAgent es) where ...
```

This can be exposed via an `Agentic.Arrow` module for users who want `(&&&)`, `(>>>)`, and arrow notation. But the core library doesn't depend on it, and examples should use the plain function style.

### The Core Primitives

```haskell
-- Send a prompt to the LLM
prompt :: (LLM :> es) => Text -> Eff es LLMResponse

-- Inject a typed value into the conversation context
inject :: (ToDhall a) => a -> ...

-- Extract a typed value from LLM output (default: Dhall)
extract :: (FromDhall a, ToDhall a, LLM :> es) => ... -> Eff es a

-- Compose: prompt >>> inject value >>> extract @TargetType
```

---

## 5. Schema Strategy

### Dhall as Default, JSON Schema as Alternative

Dhall is the library's differentiator and the more powerful option — sum types, let bindings, type-safe serialisation with `FromDhall`/`ToDhall` derivable via `Generic`. It should be the default.

JSON Schema is available for contexts where LLM support is better or users prefer it, but it's the secondary path.

### Extraction API

```haskell
-- Default: uses Dhall
extract :: (FromDhall a, ToDhall a, LLM :> es) => ... -> Eff es a

-- Explicit serializer selection
extractWith :: (LLM :> es) => SchemaFormat a -> ... -> Eff es a

-- Convenience aliases that partially apply the format
extractDhall :: (FromDhall a, ToDhall a, LLM :> es) => ... -> Eff es a
extractJson  :: (FromJSON a, ToJSON a, HasJSONSchema a, LLM :> es) => ... -> Eff es a
```

Note that the constraints differ per format — `extractDhall` requires `FromDhall`/`ToDhall`, `extractJson` requires `FromJSON`/`ToJSON`/`HasJSONSchema`. The `extract` synonym defaults to Dhall, so the common case is:

```haskell
-- This uses Dhall — the constraints are FromDhall + ToDhall
run (prompt >>> extract @Joke) "tell me a joke"

-- Explicitly request JSON Schema for this extraction
run (prompt >>> extractJson @Joke) "tell me a joke"
```

### SchemaFormat as a GADT

```haskell
data SchemaFormat a where
  Dhall :: (FromDhall a, ToDhall a) => SchemaFormat a
  Json  :: (FromJSON a, ToJSON a, HasJSONSchema a) => SchemaFormat a
```

This captures the constraints at the value level so `extractWith` can dispatch on format without the caller needing to satisfy both sets of constraints. Only the constraints relevant to the chosen format are required.

### Schema Derivation

Users derive and go:

```haskell
data Joke = Joke
  { genre     :: Text
  , setup     :: Text
  , punchline :: Text
  }
  deriving (Generic, Show, FromDhall, ToDhall)    -- for Dhall (default)
  deriving (FromJSON, ToJSON) via ...              -- for JSON (optional)
```

The `inject` function follows the same pattern — `inject` defaults to Dhall, `injectJson` for JSON.

---

## 6. Retry Mechanism

This is critical for production use. LLM structured output fails frequently, especially with Dhall.

### Design

Retry is built into `extract`. On schema parse failure, the library automatically retries up to 3 times, feeding the typed parse error back to the LLM each time. No configuration surface for now — this is a sensible default, and the DX of the extraction API shouldn't be burdened with retry plumbing until real usage patterns emerge.

```haskell
-- extract already retries internally — the caller doesn't think about it
extract :: (FromDhall a, ToDhall a, LLM :> es, Error SchemaError :> es, AgentEvents :> es)
        => ... -> Eff es a
```

**When extraction fails:**
1. Capture the typed parse error (`SchemaError` — Dhall type error, JSON validation error, etc.)
2. Render the error and append it to the conversation as context for the LLM
3. Emit a `RetryAttempt` event (with typed `SchemaError` as the reason)
4. Retry up to 3 attempts total
5. On final failure, raise `Error SchemaError`

Retry configuration (max attempts, backoff, per-agent overrides) is deferred to post-v1.

This is one of the most important features for real-world use. The prototype demos work because the README only shows successes.

---

## 7. Error Handling

Use effectful's `Error` effect throughout, not IO exceptions. Errors should be **typed precisely** — no `Text` catch-alls. And crucially, **infrastructure errors and domain errors live at different layers**.

### Infrastructure Errors (internal to the library)

These are raised by `extract`, `prompt`, and tool dispatch. Individual agents should NOT see these in their signatures — they are infrastructure concerns.

```haskell
-- Schema parse failures carry the typed parse error from the relevant parser
data SchemaError
  = DhallParseError Dhall.ParseError
  | DhallTypeError Dhall.TypeError
  | JsonParseError JSONPath String      -- aeson's typed error
  | JsonSchemaViolation SchemaViolation -- specific violation details
  deriving (Show)

-- LLM call failures carry the HTTP-level or provider-level error
data LLMError
  = HttpError HttpException
  | ProviderError StatusCode ResponseBody  -- provider returned a non-2xx
  | RateLimited RetryAfter
  | TokenLimitExceeded TokenCount TokenCount  -- used, limit
  deriving (Show)

-- Tool dispatch failures
data ToolError e
  = ToolNotFound ToolName
  | ToolInputDecodeFailed SchemaError
  | ToolExecutionFailed e  -- the tool's own typed error
  deriving (Show)
```

These are handled by the top-level runner, not by individual agents. If the LLM is down or Dhall parsing fails after 3 retries, that's not something an agent can meaningfully recover from.

### Domain Errors (agent-specific)

Agents declare their own domain-specific error types that describe *their* failure scenarios:

```haskell
data DinoResearchError
  = DinoNotFound Text
  | InsufficientSources Natural
  deriving (Show)

researchDino
  :: (LLM :> es, Error DinoResearchError :> es)
  => Agent es Text Dino
```

The agent's type signature tells you what *this agent* can fail with — not that the LLM might be down or that Dhall parsing might fail (every agent has those risks; listing them is noise).

### The Principle

**Infrastructure errors** (`SchemaError`, `LLMError`) are handled at the runner/composition level — the same place that interprets the `LLM` effect. **Domain errors** are declared by agents and handled by their callers. If an error carries a `Text` field, that's a code smell — there's almost certainly a more precise type available from the underlying library (dhall, aeson, http-client) that should be used instead.

---

## 8. Tool Use

Tools must be fully typed. A tool is a function from a typed input to a typed output, with schema derivation handled by the same machinery as `extract`/`inject`.

### Typed Tool Definition

```haskell
data Tool es where
  MkTool
    :: (FromDhall input, ToDhall input, ToDhall output, FromDhall output)
    => { toolName        :: ToolName
       , toolDescription :: Text
       , toolExecute     :: input -> Eff es output
       }
    -> Tool es
```

The existential hides the concrete `input`/`output` types but the GADT captures their constraints. The schema for the tool's input is derived from the type at tool registration time — not from `Value`, not from hand-written JSON.

### Tool Registration

```haskell
data ToolName = ToolName Text
  deriving (Eq, Ord, Show)

-- Build a tool from a typed function
mkTool
  :: (FromDhall i, ToDhall i, ToDhall o, FromDhall o)
  => ToolName -> Text -> (i -> Eff es o) -> Tool es

-- Example
data WeatherRequest = WeatherRequest { city :: Text, units :: TemperatureUnit }
  deriving (Generic, FromDhall, ToDhall)

data WeatherResponse = WeatherResponse { temp :: Double, conditions :: Text }
  deriving (Generic, FromDhall, ToDhall)

weatherTool :: (Http :> es) => Tool es
weatherTool = mkTool
  (ToolName "get_weather")
  "Get current weather for a city"
  (\(req :: WeatherRequest) -> fetchWeather req.city req.units)
```

### Tool Dispatch

When the LLM returns a tool call, the dispatcher matches on `ToolName`, deserialises the input using the tool's schema, executes the typed function, and serialises the output back. The round-trip is:

```
LLM raw output -> deserialise via tool's input type -> execute typed function -> serialise via output type -> feed back to LLM
```

No `Value` at any point in the user-facing API. The internal dispatch may use an intermediate representation for the LLM wire format, but that's behind the interpreter boundary.

---

## 9. Observability

Replace the `Writer Events` from the monolithic RWS with a typed events effect. Every event should carry structured, typed data — not `Text` descriptions or `Value` blobs.

### Event Types

```haskell
data AgentEvents :: Effect where
  Emit :: Event -> AgentEvents m ()

data Event
  = LLMCallStarted LLMRequest
  | LLMCallCompleted LLMResponse Duration
  | LLMCallFailed LLMError
  | SchemaExtractAttempt
      { attempt     :: Natural
      , schemaFormat :: SchemaFormatTag  -- Dhall | Json
      , targetType  :: TypeRep
      }
  | SchemaExtractSuccess
      { schemaFormat :: SchemaFormatTag
      , targetType  :: TypeRep
      }
  | SchemaExtractFailed SchemaError
  | ToolDispatched ToolName
  | ToolSucceeded ToolName Duration
  | ToolFailed ToolName (ToolError SomeException)  -- existential for the tool's error
  | RetryAttempt
      { attempt     :: Natural
      , maxAttempts :: Natural
      , reason      :: SchemaError       -- the parse error that triggered the retry
      }
```

Note:
- `LLMCallFailed` carries `LLMError`, not `Text`.
- `SchemaExtractFailed` carries `SchemaError`, not `Text`.
- `ToolFailed` carries `ToolError`, not a description string.
- `ToolDispatched` carries `ToolName` (a typed newtype), not `Text`.
- Duration, attempt counts, and type representations are all typed precisely.

### Interpreters

```haskell
runEventsStdout :: (IOE :> es) => Eff (AgentEvents : es) a -> Eff es a
runEventsNoop   :: Eff (AgentEvents : es) a -> Eff es a

-- Users can write their own for OpenTelemetry, structured logging, etc.
-- The typed Event ADT makes this straightforward — pattern match, don't parse strings.
```

---

## 10. Migration & Prioritisation

### Phase 1: Foundation (do this first)
1. Set up module namespace structure and cabal file
2. Define `Agent es a b = a -> Eff es b` and core effect types (`LLM`, `AgentConfig`, `AgentEvents`, `AgentSession`)
3. Define typed error ADTs (`SchemaError`, `LLMError`, `ToolError`)
4. Port one provider interpreter (Anthropic)
5. Get `prompt >>> extract @Text` working end-to-end with the new architecture (Dhall path)

### Phase 2: Schema & Extraction
6. Port Dhall extraction as the default `extract` path
7. Port JSON Schema extraction as `extractJson`
8. Implement `extractWith` and the `SchemaFormat` GADT
9. Implement retry mechanism with typed error feedback
10. Wire up `Error SchemaError` and `Error LLMError` effects

### Phase 3: Composition & Tools
11. Implement fanout / parallel combinators over plain `Agent` type
12. Implement fully typed `Tool` GADT with schema derivation
13. Implement tool dispatch loop (deserialise → execute → serialise, no `Value` in user API)
14. Ensure the dinosaur project example works end-to-end

### Phase 4: Polish
15. Add typed observability `AgentEvents` effect
16. Haddock documentation for all public modules
17. Property-based tests for schema derivation round-trips
18. Integration tests (with a mock LLM interpreter — another win of the effect approach)
19. CI pipeline
20. Complete cabal metadata, CHANGELOG, README rewrite

### Post-v1
- Arrow wrapper module (`Agentic.Arrow`) with newtype + `Arrow`/`ArrowChoice` instances
- Provider extension effects (`AnthropicExt`, `OpenAIExt`) for frontier features
- Extract `Agentic.Provider.*` into separate packages (`haskell-agentic-anthropic`, `haskell-agentic-openai`)
- Streaming support

### Testing Note

The effect architecture gives you a major testing advantage: define a mock interpreter:

```haskell
runLLMMock :: [(LLMRequest -> LLMResponse)] -> Eff (LLM : es) a -> Eff es a
```

This lets you test agent logic, retry behaviour, composition, and error handling without ever hitting a real LLM. The prototype has no tests partly because testing against live LLMs is expensive and flaky. The effect boundary eliminates that excuse entirely.

---

## Summary of Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Effect library | `effectful-core` | Performance, ergonomics, ecosystem momentum |
| LLM boundary | Core `LLM` effect, provider extensions deferred post-v1 | Portable by default, extension point designed but not shipped yet |
| Package structure | Single package, split module namespaces | Prove boundaries first, extract packages later |
| Default schema | Dhall | More powerful (sum types, let bindings), derivable via Generic, library's differentiator |
| JSON Schema | Available via `extractJson` | Secondary path for broader LLM compatibility |
| Agent core | Plain functions `a -> Eff es b` | Simpler, Arrow wrappable later via newtype + typeclass instance |
| Error handling | Typed error ADTs via `Error` effect | No `Text` catch-alls — use the actual error types from dhall/aeson/http-client |
| Tools | Fully typed GADT, no `Value` anywhere | Schema derived from input/output types, same machinery as extract/inject |
| Retry | Built-in with error feedback | Critical for production structured output |
| Observability | Typed `Event` ADT via `AgentEvents` effect | All event fields are typed — `LLMError` not `Text`, `ToolName` not `String` |
| Composition | TBD — Arrow instance as optional post-v1 wrapper | `Agentic.Arrow` module can expose `(&&&)`, `(>>>)` over a newtype |