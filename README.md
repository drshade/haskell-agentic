
# Examples

```haskell
ghci> runKleisli (prompt >>> roundtripAsDhall @Joke) "a quick joke"
ghci> runKleisli (prompt >>> roundtripAsDhall @[Joke]) "a few quick jokes"
ghci> runKleisli (prompt >>> roundtripAsDhall @[BetterJoke]) "a few quick jokes"
ghci> runKleisli (prompt >>> roundtripAsDhall @[(BetterJoke, Joke)]) "a few quick jokes"

ghci> runKleisli (prompt >>> roundtripAsDhall @[Task]) "3 important tasks when planning a vacation"
```