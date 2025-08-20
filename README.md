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
data BetterJoke
    = DadJoke { setup :: Text, punchline :: Text }
    | OneLiner { line :: Text }
    | Story { paragraphs :: [Text] }
    deriving (Generic, Show, FromDhall, ToDhall)
```

We can convince the LLM to reply to use conforming to this type:

```haskell
ghci> runKleisli (prompt >>> roundtripAsDhall @BetterJoke) "a joke please"
OneLiner {line = "I'm reading a book about anti-gravity; it's impossible to put down."}
```

Or whatever we want! How about a list of them?

```haskell
ghci> runKleisli (prompt >>> roundtripAsDhall @[BetterJoke]) "a few quick jokes"
[DadJoke {setup = "Why did the scarecrow win an award?", 
          punchline = "Because he was outstanding in his field."},
 OneLiner {line = "I'm reading a book about anti-gravity - it's impossible to put down."},
 Story {paragraphs = ["I tried to catch some fog yesterday.", "I mist."]}]
```

Or just a plain old string? No problemo:

```haskell
ghci> runKleisli (prompt >>> roundtripAsDhall @Text) "a joke please"
"Why did the scarecrow win an award? Because he was outstanding in his field."
```

Whats the funniest number?

```haskell
ghci> runKleisli (prompt >>> roundtripAsDhall @Int) "a joke please"
42
```

42 of course!

The system teaches the LLM Dhall syntax through examples and constrains outputs to match the expected schema, eliminating fragile string parsing and runtime type errors common in JSON-based approaches.

### Others examples...

```haskell
ghci> runKleisli (prompt >>> roundtripAsDhall @Joke) "a quick joke"
ghci> runKleisli (prompt >>> roundtripAsDhall @[Joke]) "a few quick jokes"
ghci> runKleisli (prompt >>> roundtripAsDhall @[BetterJoke]) "a few quick jokes"
ghci> runKleisli (prompt >>> roundtripAsDhall @[(BetterJoke, Joke)]) "a few quick jokes"

ghci> runKleisli (prompt >>> roundtripAsDhall @[Task]) "3 important tasks when planning a vacation"
```