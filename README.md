# Agentic Haskell

WORK IN PROGRESS

## Dhall-Typed LLM Outputs

This research explores using Dhall as a schema language for structured LLM outputs. By leveraging Dhall's type system and Haskell's type classes, we can get LLMs to generate responses that parse directly into strongly-typed Haskell values.

### Key Features

- **Type-safe LLM responses**: Claude generates valid Dhall expressions that are automatically parsed into Haskell data types
- **Automatic schema generation**: Dhall schemas are derived from Haskell types via `FromDhall`/`ToDhall` instances
- **Complex type support**: Handles sum types, product types, optionals, lists, and nested structures
- **Compositional API**: Simple arrow-based interface (`prompt >>> roundtripAsDhall @MyType`)

### Example

Given this data type:

```haskell
data Joke = Joke
    { genre     :: Text
    , setup     :: Text
    , punchline :: Text
    }
    deriving (Generic, Show, FromDhall, ToDhall)
```

We can convince the LLM to reply to us conforming to this type:

```haskell
ghci> run (prompt >>> extract @Joke) "a joke please"
Joke {genre = "Dad joke", setup = "Why did the scarecrow win an award?", punchline = "Because he was outstanding in his field."}
```

How about a list of them?

```haskell
ghci> run (prompt >>> extract @[Joke]) "a few jokes please!"
[ Joke {genre = "Dad", setup = "I'm reading a book about anti-gravity.", punchline = "It's impossible to put down."}
, Joke {genre = "One-liner", setup = "I told my wife she was drawing her eyebrows too high.", punchline = "She looked surprised."}
, Joke {genre = "Programming", setup = "Why do programmers prefer dark mode?", punchline = "Because light attracts bugs."}
, Joke {genre = "Animal", setup = "What do you call a fish with no eyes?", punchline = "Fsh."}
, Joke {genre = "Pun", setup = "I would tell you a construction joke...", punchline = "But I'm still working on it."}
]
```

Or just a plain old string?

```haskell
run (prompt >>> extract @Text) "a funny joke please!"
"Why don't scientists trust atoms? Because they make up everything!"
```

Whats the funniest number?

```haskell
ghci> run (prompt >>> extract @Int) "a funny joke please!"
42
```

How about something more complex? Lets make a better joke structure with different constructors:

```haskell
data BetterJoke
    = DadJoke { setup :: Text, punchline :: Text }
    | OneLiner { line :: Text }
    | Story { paragraphs :: [Text] }
    | KnockKnock { whosThere :: Text, punchline :: Text }
    deriving (Generic, Show, FromDhall, ToDhall)
```

No problemo for our LLM:

```haskell
ghci> run (prompt >>> extract @BetterJoke) "an haskell joke please!"
DadJoke {setup = "Why do Haskell programmers prefer pure functions?", punchline = "Because they don't like side effects."}
```

How about injecting an object, and have it convert to another?

```haskell
ghci> run (prompt >>> inject (Joke "knockknock" "knock knock? whos there? boo! boo who?" "don't cry it's only a joke!") >>> extract @BetterJoke) "convert this!"
KnockKnock {whosThere = "boo!", punchline = "don't cry it's only a joke!"}
```

Typesafe prompt responses FTW!

The system teaches the LLM Dhall syntax through examples and constrains outputs to match the expected schema, eliminating fragile string parsing and runtime type errors common in JSON-based approaches.

### Others examples...

```haskell
-- This one is cool - returning a tuple
ghci> run (prompt >>> extract @[(BetterJoke, Joke)]) "a few quick jokes"

-- Another example using Task
ghci> run (prompt >>> extract @[Task]) "3 important tasks when planning a vacation"
```