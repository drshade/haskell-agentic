# haskell-agentic

Composable agentic workflows in Haskell: typed steps, mixing LLMs and
[Jev](https://docs.typesafe.ai), that you can inspect before you run them.

> **Status:** v2 in progress. The core (`agentic/`) and three providers work,
> each with live tests: Jev as System One (`agentic-jev`), and Claude
> (`agentic-anthropic`) and OpenAI (`agentic-openai`) as System Two, or as System
> One through `viaLLM`. `agentic-io` has `concurrently` and `loadDotEnv`;
> recording and replay aren't written yet. `cabal run dino` and
> `cabal run tictactoe` run the two examples with the real models.

## The idea

A workflow is an `Agentic m i o`: a typed description of how to get from an `i`
to an `o`, running in some effect `m`. We call these values flows. You build them
out of steps and compose them with the usual `Arrow` combinators (`>>>`, `&&&`,
`|||`). Nothing runs until you hand a flow to an interpreter.

There are four kinds of step:

| Step | What it does | Returns |
|---|---|---|
| `draft` | an LLM writes a value, optionally using tools along the way | any `Contract o` |
| `judge` | Jev answers typed questions about the input | answers with calibrated probabilities |
| `act` | plain code, with any effect `m` | whatever it returns |
| `arr` | a pure function: glue between steps | whatever it returns |

Because a flow is data, you can `describe` it (print its structure, or render a
diagram) before spending a single token. Because the interpreter is separate, you
can run the same flow against real providers, a mock, or a recording.

Under the hood there are two types. `Step` is the leaves, which do the work.
`Agentic` is the structure, which wires the leaves together:

```haskell
data Step m i o where
  Arr   :: (i -> o)                                            -> Step m i o
  Act   :: (i -> m o)                                          -> Step m i o
  Draft :: (Contract i, Contract o) => Instruction -> [Tool m] -> Step m i o
  Judge :: Contract i => Questions o                           -> Step m i o

data Agentic m i o where
  Step   :: Step m i o                     -> Agentic m i o
  Seq    :: Agentic m a b -> Agentic m b c -> Agentic m a c
  Fanout :: Agentic m a b -> Agentic m a c -> Agentic m a (b, c)
  First  :: Agentic m a b                  -> Agentic m (a, c) (b, c)
  Choose :: Agentic m a c -> Agentic m b c -> Agentic m (Either a b) c
  Each   :: Agentic m a b                  -> Agentic m [a] [b]
  Note   :: Note -> Agentic m i o          -> Agentic m i o
```

`Agentic m` has `Category`, `Arrow` and `ArrowChoice` instances. `arr` is
`Step . Arr`, and `&&&`, `***`, `|||` and `+++` are overridden to build their own
constructors, so `describe` sees "these run side by side", not a tangle of
`arr swap`. There's deliberately no `ArrowApply` and no `Monad`: either would let
a flow pick its next step from a runtime value, and then it couldn't be described
without running it.

## Jokes

```haskell
data Joke = Joke { genre :: Text, setup :: Text, punchline :: Text }
  deriving (Generic, Show, Contract)

ghci> run (draft @Joke "a joke please") ()
Joke {genre = "Dad joke", setup = "Why did the scarecrow win an award?", punchline = "Because he was outstanding in his field."}
```

The step's input is the model's context. You never have to inject it yourself:

```haskell
data BetterJoke
  = DadJoke    { setup :: Text, punchline :: Text }
  | OneLiner   { line :: Text }
  | KnockKnock { whosThere :: Text, punchline :: Text }
  deriving (Generic, Show, Contract)

ghci> run (draft @BetterJoke "convert this joke") (Joke "knock-knock" "Knock knock. Who's there? Boo." "Don't cry, it's only a joke!")
KnockKnock {whosThere = "Boo", punchline = "Don't cry, it's only a joke!"}
```

`Contract` is derived from the type. Sum types, records, lists and `Maybe` all work.

## Contracts

A `Contract a` is a two-way codec with documentation. It says how to show an `a`
to a model, how to read one back, and what the schema looks like. When you want
descriptions, and you usually do because they make a big difference to output
quality, write the contract out in the applicative codec style:

```haskell
instance Contract Joke where
  contract = record "A joke, split into its parts" $ Joke
    <$> required "genre"     "The style of joke, e.g. pun, dad joke" genre
    <*> required "setup"     "The setup line"                        setup
    <*> required "punchline" "The line that lands it; no explanation" punchline

instance Contract BetterJoke where
  contract = sumOf "A joke in one of several shapes"
    [ constructor "DadJoke"    "A setup and a groan-worthy punchline" dadJoke
    , constructor "OneLiner"   "A single line"                        oneLiner
    , constructor "KnockKnock" "The classic call-and-response"        knockKnock ]
```

The schema and the decoder come from the same definition, so they can't disagree.
Deriving (`deriving (Generic, Contract)`) builds the same thing without
descriptions, and `genericContract & field "punchline" "..."` adds descriptions to
a derived contract. Field names are checked against the real fields before the
flow runs.

Contracts compile to the providers' native mechanisms rather than to prompt text:

- a step's output type becomes the provider's structured-output schema
- a tool's input type becomes a native tool definition with strict schema checking

So a reply that doesn't match the schema should never happen. Contracts can also
carry checks the wire schemas can't express, like `between 1 10` or a length
limit. Those are stated in the description and checked locally. A failed check
goes back to the model and it tries again.

The state's types are part of what the model reads. Field names carry meaning:
a meeting note wrapped in a record with `setup` and `punchline` fields looks
like a joke before the model reads a word. (Jev rated one 0.67 "a joke" that
way, and 0.02 as plain text.) Give each step the state it should judge, and no
more.

For enumerations, the same descriptions reach Jev (see `Options` below). A type
is described once, and both kinds of model see the same wording.

## Is it actually funny?

An LLM is good at writing. Jev is good at judging quickly, with a probability you
can put a threshold on.

```haskell
funny :: Questions YesNo
funny = yesNo "Would a 10-year-old laugh at this joke?"

ghci> run (draft @[Joke] "ten jokes please" >>> keep 0.7 funny) ()
[Joke {...}, Joke {...}, Joke {...}]
```

`keep p q` is an ordinary flow, `Agentic m [i] [i]`. It judges every item and keeps the
ones where the probability of yes is at least `p`. Its sibling
`gate p q :: Agentic m i (Either i i)` sends one input down the `Right` branch if it
passes and the `Left` branch if it doesn't:

```haskell
kidFriendly :: Agentic m Joke Joke
kidFriendly = gate 0.9 (yesNo "Is this joke suitable for a 10-year-old?")
          >>> (draft "rewrite this joke for a 10-year-old" ||| returnA)
```

`yesNo` is Jev's Noul primitive. Jev also has `choice` (pick one constructor of an
enumeration) and `score` (a position on ordered levels). Each comes back with its
probabilities:

```haskell
data Groan = Mild | Solid | Unbearable deriving (Generic, Show)

instance Options Groan where
  options = described "How much the audience groans"
    [ option Mild       "A polite smile; most people didn't notice"
    , option Solid      "An audible groan from most of the room"
    , option Unbearable "People get up and leave" ]

deriving via Enumeration Groan instance Contract Groan

groan :: Questions (Score Groan)
groan = score "How much will the audience groan?"
```

`choice` and `score` need an `Options` type: an enumeration whose options each
have a description. For `score`, the list order is the level order, lowest
first. Answers decode back to real values, so `Choice Groan` holds a `Groan` and
its probabilities are keyed by `Groan`. `Enumeration` gives the type a `Contract`
from the same options, so an LLM drafting a `Groan` sees exactly the descriptions
Jev sees. `deriving (Generic, Options)` works too: it uses constructor names as
labels, with no descriptions.

Following Jev's terms, the step's input is the *state* and the step asks it
`Questions`. One question is just `Questions` of size one, and independent
questions about the same state compose applicatively into one request:

```haskell
review :: Agentic m Joke Review
review = judge (Review <$> funny <*> groan)
```

## Tools

Give a `draft` step some tools and it becomes an agent. The model can call them
as often as it likes, and each result goes back into the step's conversation. The
step finishes when the model responds with a value of the step's output type.

```haskell
research :: Agentic IO Text Dino
research = draftWith [fossilSearch, reliable] "Research this dinosaur. Cite a source for every claim."

fossilSearch :: Tool IO
fossilSearch = tool "search" "Search the fossil database" (act searchFossils)

reliable :: Tool IO
reliable = tool "is_reliable" "Is this source trustworthy?" (judge (yesNo "Is this a reliable scientific source?"))
```

A tool's body is just an `Agentic`. It can be an effect, a Jev judgement, a pipeline,
or another agent. That's the whole mechanism. Anything more (asking a human
before a destructive tool, capping turns) is built by you, out of the same pieces:

```haskell
deleteRecord :: Tool IO
deleteRecord = tool "delete" "Delete a fossil record" (act confirmWithHuman >>> act deleteIfApproved)
```

### How the loop works

The core runs the loop. A provider only ever takes one turn at a time, which is
why the loop behaves the same against a real provider, a mock or a replay.

1. The model sees the instruction, the step's input (encoded by its contract), the
   tool definitions and the output schema.
2. If it calls tools, they run (concurrently, if the runtime allows). Each result
   is added to the step's conversation, and the loop takes another turn.
3. When it gives a final value, the value is decoded and checked against the
   output contract. If that passes, the step returns it. If not, the problem is
   added to the conversation and the loop goes round again.

When a tool is called, the library looks it up by name, decodes the model's input
with the tool's contract, runs the tool's body with the same runtime, and encodes
the result. An unknown tool name or an input that doesn't decode is reported back
to the model. If the body contains a `draftWith`, that's a separate conversation:
a sub-agent with its own context.

A few consequences:

- **There's no turn limit.** The model decides when it's done. If you want a
  cap, wrap the runtime (`capped 20`).
- **Failures in a tool's body escape the step**, like any other error in `m`. If
  you want the model to see a failure, give the tool an output type that says so,
  such as `Either NotFound Fossil`.
- **The conversation stays inside the step.** Only the typed result moves on.
  Everything that happened is available to the runtime's `observe` hook.
- **The history is append-only**, and each provider's own messages are kept
  unchanged. Providers need their messages (thinking blocks, reasoning items)
  sent back exactly as they were produced.

## The dino project

A grade 5 project, and a flow that mixes both kinds of model. Claude suggests ten
prehistoric creatures. Jev sorts the dinosaurs from the rest; pterosaurs and
plesiosaurs are the classic "not actually dinosaurs". Code keeps the clear
dinosaurs. Claude draws each one and makes its trump card, then makes a poster
with a corner for the creatures that weren't dinosaurs.

```haskell
dinoProject :: Agentic IO () Poster
dinoProject =
  draft @[Creature] "Name 10 prehistoric creatures a grade 5 class might have heard of. Include a mix of kinds, not only dinosaurs."
    >>> each classify
    >>> arr (partition (clearly Dinosaur 0.8)) `named` "split off the clear dinosaurs (≥ 0.8)"
    >>> (each (arr fst >>> exhibit) *** arr (map notADinosaur) `named` "note what the others were")
    >>> arr (uncurry Exhibit)
    >>> draft @Poster "Create a poster of these dinosaurs for a grade 5 class. Add a corner about the creatures that weren't dinosaurs, and what they were."

-- Jev decides what kind of animal each creature was.
classify :: Agentic IO Creature (Creature, Choice Kind)
classify = returnA &&& judge (choice "What kind of animal was this creature?")

exhibit :: Agentic IO Creature Entry
exhibit =
  (returnA &&& draft @DinoPic "Draw an ascii picture of this dinosaur, 10 lines high"
           &&& draft @TrumpCard "Make a trump card for this dinosaur")
    `named` "exhibit"
    >>> arr (\(c, (p, t)) -> Entry c p t)
```

`Kind` is an `Options` type, and each option's description tells Jev what it
means ("A flying reptile, such as Pteranodon. Not a dinosaur."). The trump card's
stats are a `Stat` type whose contract says "From 1 (lowest) to 10 (highest)" and
checks it, so every card uses the same scale. The poster is drafted from a named
`Exhibit` record rather than a tuple, so Claude sees `dinosaurs` and
`notDinosaurs`, not `_1` and `_2`. The whole example is in `examples/Dino.hs`, and
`cabal run dino` runs it with Claude and Jev.

`each` maps a flow over a list, and the interpreter is free to run the items
concurrently. `&&&` runs flows side by side on the same input, `***` runs one
flow on each half of a pair, and `|||` picks a branch. All of these are ordinary
`Arrow` and `ArrowChoice` combinators.

### Describe it before you run it

```
ghci> describe dinoProject
draft [Creature]  "Name 10 prehistoric creatures a grade 5 class might have heard of. Include a mix of kinds, not only dinosaurs."
each
└─ judge choice of 7 "What kind of animal was this creature?"  (keeping its input)
split off the clear dinosaurs (≥ 0.8)
both halves
├─ first → each
│  └─ exhibit  together  (keeping its input)
│     ├─ draft DinoPic  "Draw an ascii picture of this dinosaur, 10 lines high"
│     └─ draft TrumpCard  "Make a trump card for this dinosaur"
└─ second → note what the others were
draft Poster  "Create a poster of these dinosaurs for a grade 5 class. Add a corner about the creatures that weren't dinosaurs, and what they were."
```

`describe` returns a `Description`, a plain data type whose `Show` instance is the
tree above. `mermaid` renders it as a flowchart, and `toValue` turns it into JSON
for UIs and other agents. You can also walk it yourself:

```haskell
describe :: Agentic m i o -> Description

data Description
  = Leaf      StepInfo
  | Sequence  [Description]           -- a >>> b >>> c, flattened
  | Together  [Description]           -- a &&& b &&& c, flattened
  | Halves    Description Description -- a *** b
  | OnFirst   Description
  | Branch    Description Description
  | ForEach   Description
  | Annotated Note Description

data StepInfo
  = Identity                          -- returnA
  | Glue                              -- arr
  | Effect                            -- act
  | DraftInfo { draftInstruction :: Instruction, draftInput, draftOutput :: Schema, draftTools :: [ToolInfo] }
  | JudgeInfo { judgeState :: Schema, judgeQuestions :: [QuestionSpec] }
```

A `Description` is simplified rather than a literal copy of the flow: chains
are flattened, and the tree view hides unnamed glue between steps. It never
hides a branch, though. Inside `&&&`, `***` and `|||` an unnamed `arr` shows as
`arr`, so you can see it's there and name it, and a `returnA` beside a step
shows as "keeping its input". Tool bodies
are expanded the first time a tool appears and referenced by name after that, so
a tool that can call itself still renders.

### Notes

`describe` already knows each draft's instruction and tools, each judgement's
questions, and every contract's schema. A pure `arr` or an `act` is opaque, so
you name it with `named`. Written infix, `named` binds as tightly as function
application, so it names exactly the expression before it:

```haskell
    >>> arr (partition (clearly Dinosaur 0.8)) `named` "split off the clear dinosaurs (≥ 0.8)"
```

Without that name, the tree would show nothing between classifying and building
exhibits, and the step that decides which creatures become exhibits would be
invisible. To name a larger
sub-flow, bracket it, as `exhibit` does above. `note name description flow`
names a flow and describes it too.

Names nest into paths like `exhibit / picture`. Tracing uses those paths, and
they stay stable when you edit the flow around them, so they also work as keys
for caching and for comparing two runs. A runtime can choose to show the model
where it is in the flow ("You're in step `exhibit`"). An instruction is written
for the model. A name is written for whoever is watching the flow.

## Tic-tac-toe

Claude plays X against an opponent written in code. The board lives in an
`IORef` and Claude's only tools are `look` and `play`. The draft step can't
finish until Claude reports an `Outcome`, and a final `act` checks that claim
against the real board.

```haskell
game :: IORef Board -> Agentic IO () (Outcome, Outcome)
game board =
  draftWith @Outcome
    [look board, play board]
    "You are X in a game of tic-tac-toe against O, and you move first. Look at the board, then play one move at a time until the game is over. Then report how it ended."
    >>> (returnA &&& act (const (actual <$> readIORef board))) `named` "check the claim"

play :: IORef Board -> Tool IO
play board = tool @Move @Text "play" "Place an X on an empty square. O replies straight away." $
  act (playAndReply board)
```

A `Move` is a row and a column, each a `Coordinate` whose contract says "From
1 to 3" and checks it. An illegal move is just a tool result that says so
("That square is taken"), and the model reads it and tries again. The library
has no special machinery for this. `cabal run tictactoe` plays a game:

```
play {"column":2,"row":2}
O . .
. X .
. . .
Your move.
…
play {"column":1,"row":3}
O O X
. X O
X . X
Game over: you won.

Claude says: Won. The board says: Won.
```

## Running flows

A runtime has two roles to fill. **System One** answers `judge` steps: fast,
typed judgements with probabilities. **System Two** answers `draft` steps: an
LLM taking turns. You pick one provider for each role:

```haskell
main :: IO ()
main = do
  rt <- pure runtime
    >>= withSystemOne jev
    >>= withSystemTwo (anthropic & model "claude-opus-5-5")
    <&> concurrently . observing logEvent
  poster <- interpret rt dinoProject ()
  print poster
```

Each provider has a default config (`jev`, `anthropic`, `openai`) that you
adjust with setters: `anthropic & model "claude-sonnet-5-5" & effort Low`. The
setters for settings providers share (`model`, `key`, `system`, `effort`,
`maxTokens`, `endpoint`, `timeout`) work on any provider's config; the rest,
like Anthropic's `fallbacks`, live in the provider's module. API keys come from
the environment (`JEV_TOKEN`, `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`) unless you
set them with `key`. To keep
keys in a file, copy `.env.example` to `.env` (git ignores it) and call
`loadDotEnv` from `agentic-io` at startup. Variables already set in the
environment win. `run`
in the examples above is `interpret` with a runtime built this way.

Jev only provides System One, so `withSystemTwo jev` is a type error. The LLM
providers can fill both roles. This runs everything on OpenAI, with no Jev token:

```haskell
rt <- pure runtime
  >>= withSystemOne (openai & model "gpt-6-astra")
  >>= withSystemTwo (openai & model "gpt-6-astra")
```

An LLM answering as System One gives probabilities, but they aren't calibrated
the way Jev's are, so a `gate 0.9` means less. Every judgement in a trace records
which provider answered it.

### Prompts and sessions

LLM providers take an optional system prompt that applies to every `draft` in
the runtime: `anthropic & system "You write for primary school children."`. A
step's `Instruction` is its task. Text shared by several steps is just a Haskell
string you reuse.

The library doesn't tell the model how to format its reply. Output schemas and
tool inputs go through the providers' native structured outputs and strict tool
schemas, which constrain the model as it generates. It can't wrap its JSON or add
commentary, so there's nothing to tell it. What the model does get is meaning:
the instruction, the encoded state, and the field descriptions from your
contracts. A refusal or a reply cut off at the token limit is a provider error,
not something to retry.

There are no sessions to manage. Steps pass typed values, so anything a later
step needs goes through the types (`research &&& returnA >>> poster`). A step's
conversation lives only for that step. Memory across runs belongs to the caller:
put it in the flow's types (`Agentic IO (History, Message) (History, Reply)`) or
behind tools that read and write a store.

### Inside the runtime

```haskell
data Runtime m = Runtime
  { systemOne :: SystemOne m                 -- JudgeRequest -> m [Answer]
  , systemTwo :: SystemTwo m                 -- Conversation -> m Turn
  , parallel  :: forall a. [m a] -> m [a]    -- default: sequence
  , observe   :: Event -> m ()               -- default: nothing
  , failure   :: forall a. FlowError -> m a  -- how the core raises its own errors
  }

interpret :: Monad m => Runtime m -> Agentic m i o -> i -> m o
```

`runtime` fills these with defaults. `parallel` is used by `each`, `&&&` and
parallel tool calls. Its default runs things one after another, so the core never
needs threads. `concurrently` (from `agentic-io`) swaps in real concurrency.
`observe` receives an event for every step, turn, tool call, tool result and
judgement, each tagged with its note path. Provider errors, like HTTP failures,
are thrown in `m` by the provider and escape the flow.

Everything else is a function from `Runtime m` to `Runtime m`:

| Modifier | What it does |
|---|---|
| `concurrently` | run independent work at the same time (from `agentic-io`; the dino example takes about half as long) |
| `observing f` | send every event to `f` |
| `cached store` | reuse answers, keyed by note path and request |
| `recording file`, `replaying file` | record calls in production, replay them in tests |
| `capped n` | fail a step after `n` turns |

For tests, swap in scripted providers. It's the same flow with no network:

```haskell
testRuntime :: Runtime IO
testRuntime = runtime { systemOne = answerAll (yes 0.95), systemTwo = scripted [respond jokes] }
```

## Layout

| Package | Depends on | Contains |
|---|---|---|
| `agentic` | `base` | `Agentic`, steps, tools, combinators, `Contract`, `Questions`, `describe`, `interpret`, `Runtime`, pure modifiers, scripted providers |
| `agentic-aeson` | `agentic`, aeson | conversions between the core's `Value` and aeson, for provider packages |
| `agentic-anthropic` | `agentic`, http, aeson | Anthropic as System Two (and System One via the LLM adapter) |
| `agentic-openai` | `agentic`, http, aeson | OpenAI as System Two (and System One), over the Responses API |
| `agentic-jev` | `agentic`, http, aeson | Jev as System One |
| `agentic-io` | `agentic`, async, directory | `concurrently` and `loadDotEnv`; recording and replay to come |
| `examples` | all of the above | everything in this README |

## Design rules

1. **A flow is a description.** Building one never runs anything, and `describe`
   never needs to run anything either.
2. **The core depends only on `base`.** Providers, HTTP and JSON live in their own
   packages. The core should also build under MicroHs.
3. **Steps pass typed values, not conversations.** A step's conversation (tool
   calls, retries) stays inside the step. Only its typed output moves on.
4. **Tools are flows.** There's no separate tool system, and no special handling of
   effects, permissions or limits. Those belong to the people writing the tools.
5. **Use the provider's native features.** Structured outputs, strict tool
   schemas and parallel tool calls come from the provider, not from prompt text.
6. **Don't second-guess providers.** Limits such as option counts, schema features
   and sizes belong to the provider and change with its models. The library passes
   requests through and reports the provider's own errors. It only checks its own
   consistency.
7. **Policy is explicit.** Jev returns probabilities and the flow decides what to
   do with them. The library never applies a hidden threshold.

## History

v0 (the Kleisli-arrow prototype, with Dhall as the output format) will be tagged
`v0-prototype` when v2 replaces `main`.
