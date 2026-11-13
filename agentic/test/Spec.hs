module Main (main) where

import Agentic
import RecursionSpec qualified
import Agentic.Schema (Field (..), Schema (..), Shape (..))
import Agentic.Scripted
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Test.Hspec hiding (describe)
import Test.Hspec qualified

-- ---------------------------------------------------------------------------
-- README types

data Joke = Joke {genre :: Text, setup :: Text, punchline :: Text}
  deriving (Generic, Show, Eq, Contract)

data BetterJoke
  = DadJoke {setup :: Text, punchline :: Text}
  | OneLiner {line :: Text}
  | KnockKnock {whosThere :: Text, punchline :: Text}
  deriving (Generic, Show, Eq, Contract)

data Groan = Mild | Solid | Unbearable
  deriving (Generic, Show, Eq)

instance Options Groan where
  options =
    documentedOptions
      "How much the audience groans"
      [ option Mild "A polite smile"
      , option Solid "An audible groan"
      , option Unbearable "People get up and leave"
      ]

instance Contract Groan where contract = enumeration

newtype Rating = Rating Int
  deriving (Show, Eq)

instance Contract Rating where
  contract = mapStructure Rating (\(Rating n) -> n) (between 1 10 contract)

data Review = Review {funny :: YesNo, groan :: Score Groan}
  deriving (Show, Eq)

documentedJoke :: Structure Joke
documentedJoke =
  record "A joke, split into its parts" $
    Joke
      <$> required "genre" "The style of joke" (.genre)
      <*> required "setup" "The setup line" (.setup)
      <*> required "punchline" "The line that lands it" (.punchline)

funny :: Questions YesNo
funny = yesNo "Would a 10-year-old laugh at this joke?"

groan :: Questions (Score Groan)
groan = score "How much will the audience groan?"

joke :: Joke
joke = Joke "pun" "Why was the scarecrow promoted?" "He was outstanding in his field."

-- ---------------------------------------------------------------------------
-- Helpers

-- | A runtime with a script for System Two and fixed answers for System One.
testRuntime :: [Action] -> Probability -> IO (Runtime IO)
testRuntime turns p = do
  two <- scripted turns
  pure runtime{systemOne = fixedAnswers p, systemTwo = two}

roundTrips :: (Eq a, Show a) => Codec a -> a -> Expectation
roundTrips c a = c.decode (c.encode a) `shouldBe` Right a

main :: IO ()
main = hspec $ do
  RecursionSpec.spec
  describe' "Contracts" $ do
    it "round-trips a derived record" $
      roundTrips modelContract joke

    it "round-trips a derived sum as tagged objects" $ do
      roundTrips modelContract (OneLiner "I'm on a seafood diet.")
      modelContract.encode (OneLiner "x")
        `shouldBe` Object [("tag", String "OneLiner"), ("line", String "x")]

    it "encodes an Options type as its labels" $ do
      modelContract.encode Solid `shouldBe` String "Solid"
      modelContract.decode (String "Unbearable") `shouldBe` Right Unbearable

    it "names derived schemas after their type" $
      (modelContract @Joke).schema.title `shouldBe` Just "Joke"

    it "keeps descriptions written in the codec" $
      case (either (error . show) id (compileModel documentedJoke)).schema.shape of
        SObject fs -> map ((.doc) . (.schema)) fs `shouldBe` map Just ["The style of joke", "The setup line", "The line that lands it"]
        other -> expectationFailure (show other)

    it "adds descriptions to a derived contract" $
      case (either (error . show) id (compileModel (field "punchline" "No explanation" (contract @Joke)))).schema.shape of
        SObject fs -> map ((.doc) . (.schema)) fs `shouldBe` [Nothing, Nothing, Just "No explanation"]
        other -> expectationFailure (show other)

    it "checks constraints the schema can't express" $ do
      (modelContract @Rating).decode (Integer 7) `shouldBe` Right (Rating 7)
      (modelContract @Rating).decode (Integer 11) `shouldBe` Left "must be between 1 and 10"

  describe' "Questions" $ do
    it "batches combined questions into one request" $
      map (\case AskYesNo _ -> "yesNo"; AskScore _ ls -> "score " <> T.pack (show (length ls)); AskChoice _ _ -> "choice" :: Text) ((Review <$> funny <*> groan).specs)
        `shouldBe` ["yesNo", "score 3"]

    it "decodes answers back to typed values" $
      decodeAnswers (Review <$> funny <*> groan) [YesNoAnswer 0.8, ScoreAnswer 1.2 [(1, 0.7), (2, 0.3)] 0.6]
        `shouldBe` Right (Review (YesNo 0.8) (Score 1.2 [(Solid, 0.7), (Unbearable, 0.3)] 0.6))

  describe' "interpret" $ do
    it "drafts a typed value" $ do
      rt <- testRuntime [respond joke] 1
      interpret rt (draft @Joke "a joke please") () `shouldReturn` joke

    it "sends a failed check back to the model and tries again" $ do
      rt <- testRuntime [Respond (Integer 42), Respond (Integer 7)] 1
      interpret rt (draft @Rating "rate this joke") joke `shouldReturn` Rating 7

    it "runs a tool loop until the model responds" $ do
      calls <- newIORef (0 :: Int)
      let lookupGenre = tool @Text @Text "genre_of" "Look up a joke's genre" (act (\t -> modifyIORef calls (+ 1) >> pure ("pun about " <> t)))
      rt <- testRuntime [callTools [("genre_of", String "scarecrows")], respond joke] 1
      interpret rt (draftWith @Joke [lookupGenre] "a joke please") () `shouldReturn` joke
      readIORef calls `shouldReturn` 1

    it "tells the model about unknown tools instead of failing" $ do
      events <- newIORef []
      rt <- testRuntime [callTools [("nope", Null)], respond joke] 1
      let rt' = observing (\e -> modifyIORef events (e.happened :)) rt
      _ <- interpret rt' (draft @Joke "a joke please") ()
      results <- readIORef events
      [r | ToolReturned _ r <- results] `shouldBe` [ToolFailed "there is no tool named nope"]

    it "judges with System One" $ do
      rt <- testRuntime [] 0.8
      interpret rt (judge funny) joke `shouldReturn` YesNo 0.8

    it "keeps items that pass" $ do
      rt <- testRuntime [] 0.8
      interpret rt (keep 0.7 funny) [joke, joke] `shouldReturn` [joke, joke]
      interpret rt (keep 0.9 funny) [joke, joke] `shouldReturn` []

    it "gates into branches" $ do
      rt <- testRuntime [respond joke{genre = "kids"}] 0.5
      let kidFriendly = gate 0.9 funny >>> (draft @Joke "rewrite this joke for a 10-year-old" ||| returnA)
      interpret rt kidFriendly joke `shouldReturn` joke{genre = "kids"}

    it "runs structure: fanout and each" $ do
      rt <- testRuntime [respond joke, respond (Rating 3)] 1
      interpret rt (draft @Joke "a joke" >>> (returnA &&& draft @Rating "rate it")) () `shouldReturn` (joke, Rating 3)
      interpret rt (each (arr (* 2))) [1, 2, 3 :: Int] `shouldReturn` [2, 4, 6]
      interpret rt (arr (+ 1) *** arr (* 2)) (1, 5 :: Int) `shouldReturn` (2 :: Int, 10)
      interpret rt (repeatUntil (>= 10) (arr (* 2))) (3 :: Int) `shouldReturn` 12
      interpret rt (repeatUntil (>= 10) (arr (* 2))) (50 :: Int) `shouldReturn` 50
      interpret rt (second (arr show)) ('a', 7 :: Int) `shouldReturn` ('a', "7")
      interpret rt (left (arr (+ 1))) (Left 1 :: Either Int Char) `shouldReturn` Left (2 :: Int)
      interpret rt (left (arr (+ 1))) (Right 'x' :: Either Int Char) `shouldReturn` (Right 'x' :: Either Int Char)

    it "fails clearly without a System One" $ do
      interpret runtime (judge funny) joke `shouldThrow` (== NoSystemOne)

  describe' "describe" $ do
    it "draws the tree without running anything" $ do
      let flow :: Agentic IO () [Joke]
          flow =
            draft @[Joke] "ten jokes please"
              >>> keep 0.7 funny
              >>> each (draftWith @Joke [tool @Text @Text "search" "Search" (act pure)] "polish this joke")
      T.lines (renderTree (described flow))
        `shouldBe` [ "draft @[Joke]  \"ten jokes please\""
                   , "keep 0.7  each"
                   , "└─ judge  yes/no \"Would a 10-year-old laugh at this joke?\"  (keeping its input)"
                   , "each"
                   , "└─ draft @Joke  \"polish this joke\""
                   , "   └─ tool search  act"
                   ]

    it "never hides a branch, even when it's only glue" $ do
      let flow :: Agentic IO (Joke, Joke) (Rating, Text)
          flow = draft @Rating "rate it" *** arr (.genre)
      T.lines (renderTree (described flow))
        `shouldBe` ["both halves", "├─ first → draft @Rating  \"rate it\"", "└─ second → arr"]

    it "draws first and second alike, and left and right alike" $ do
      let d = draft @Rating "rate it"
          tree :: Agentic IO i o -> [Text]
          tree = T.lines . renderTree . described
      tree (first d :: Agentic IO (Joke, Int) (Rating, Int)) `shouldBe` ["both halves", "├─ first → draft @Rating  \"rate it\"", "└─ second → pass"]
      tree (second d :: Agentic IO (Int, Joke) (Int, Rating)) `shouldBe` ["both halves", "├─ first → pass", "└─ second → draft @Rating  \"rate it\""]
      tree (left d :: Agentic IO (Either Joke Int) (Either Rating Int)) `shouldBe` ["branch", "├─ left → draft @Rating  \"rate it\"", "└─ right → pass"]

    it "puts a description under the name in diagrams, not in the tree" $ do
      let flow :: Agentic IO Joke Joke
          flow = note "polish" "Tidy the wording" (draft @Joke "polish it" >>> note "check" "Make sure it's still a joke" (arr id))
          d = described flow
      any (T.isInfixOf "Tidy the wording") (T.lines (mermaid d)) `shouldBe` True
      any (T.isInfixOf "act\\ncheck\\nMake sure") (T.lines (dot d)) `shouldBe` False
      any (T.isInfixOf "arr\\ncheck\\nMake sure it's still a joke") (T.lines (dot d)) `shouldBe` True
      any (T.isInfixOf "Tidy the wording") (T.lines (renderTree d)) `shouldBe` False

    it "groups a parallel branch that's several steps in a row" $ do
      let flow :: Agentic IO Joke (Joke, Rating)
          flow = (draft @Joke "rewrite it" >>> draft @Joke "shorten it") &&& draft @Rating "rate it"
      T.lines (renderTree (described flow))
        `shouldBe` [ "together"
                   , "├─ in order"
                   , "│  ├─ draft @Joke  \"rewrite it\""
                   , "│  └─ draft @Joke  \"shorten it\""
                   , "└─ draft @Rating  \"rate it\""
                   ]

    it "shows a loop and what it runs" $ do
      let flow :: Agentic IO Joke Joke
          flow = repeatUntil ((== "kids") . (.genre)) (draft @Joke "make it more kid-friendly") `named` "polish until it's for kids"
      T.lines (renderTree (described flow))
        `shouldBe` ["polish until it's for kids  repeatUntil", "└─ draft @Joke  \"make it more kid-friendly\""]

    it "draws a fork that joins again in mermaid" $ do
      let flow :: Agentic IO Joke (Joke, Rating)
          flow = returnA &&& draft @Rating "rate it"
          edges = filter (T.isInfixOf "-->") (T.lines (mermaid (described flow)))
      edges `shouldBe` ["  input --> n0", "  input --> output", "  n0 --> output"]

    it "wires second to the second half of a known pair" $ do
      let flow :: Agentic IO Joke (Joke, Rating)
          flow = (draft @Joke "retell it" &&& draft @Rating "rate it") >>> second (draft @Rating "rate it again")
          edges = filter (T.isInfixOf "-->") (T.lines (mermaid (described flow)))
      edges `shouldBe` ["  input --> n0", "  input --> n1", "  n1 --> n2", "  n0 --> output", "  n2 --> output"]

    it "joins a pass-through from both sides of a choice with one edge" $ do
      let flow :: Agentic IO (Either Joke Joke) (Joke, Rating)
          flow = (returnA &&& draft @Rating "rate it") ||| (returnA &&& draft @Rating "rate it harshly")
          edges = filter (T.isInfixOf "-->") (T.lines (mermaid (described flow)))
      edges `shouldBe` ["  input -->|left| n0", "  input -->|right| n1", "  input --> output", "  n0 --> output", "  n1 --> output"]

    it "follows takeSecond to the half it keeps" $ do
      let flow :: Agentic IO Joke Rating
          flow = (draft @Joke "retell it" &&& draft @Rating "rate it") >>> takeSecond >>> draft @Rating "rate it again"
          edges = filter (T.isInfixOf "-->") (T.lines (mermaid (described flow)))
      edges `shouldBe` ["  input --> n0", "  input --> n1", "  n1 --> n2", "  n2 --> output"]

    it "lets a loop's input leave without going through the body" $ do
      let flow :: Agentic IO Joke Joke
          flow = repeatUntil ((== "kids") . (.genre)) (draft @Joke "make it kid-friendly") >>> act pure `named` "show it"
          edges = filter (T.isInfixOf "-->") (T.lines (mermaid (described flow)))
      edges `shouldBe` ["  input --> n1", "  input --> n2", "  n1 --> n2", "  n2 --> output"]

    it "draws the same graph as DOT, with boxes as clusters" $ do
      let flow :: Agentic IO [Joke] [Joke]
          flow = each (draft @Joke "polish it") `named` "polish"
          out = mermaid (described flow)
          dotOut = T.lines (dot (described flow))
      filter (T.isInfixOf "->") dotOut `shouldBe` ["  input -> n2;", "  n2 -> output;"]
      length (filter (T.isInfixOf "subgraph cluster_") dotOut) `shouldBe` 2
      length (filter (T.isInfixOf "subgraph ") (T.lines out)) `shouldBe` 2

    it "sends a loop's again edge back to the body's first step, in both formats" $ do
      let flow :: Agentic IO Joke Joke
          flow = repeatUntil ((== "kids") . (.genre)) (draft @Joke "make it kid-friendly" >>> act pure `named` "show it")
          d = described flow
      filter (T.isInfixOf "again") (T.lines (mermaid d)) `shouldBe` ["  n2 -.->|again| n1"]
      filter (T.isInfixOf "again") (T.lines (dot d)) `shouldBe` ["  n2 -> n1 [label=\"again\", style=dashed];"]

    it "describes an explicit recursive tool with a finite reference" $ do
      let researcher = recursiveTool $ \self -> tool @Text @Text "research" "Research deeper" (draftWith [self] "research this")
          flow = draftWith @Text @Text @IO [researcher] "start"
          tree = renderTree (described flow)
      tree `shouldSatisfy` T.isInfixOf "recursive tools  (entry research)"
      tree `shouldSatisfy` T.isInfixOf "tool research  (recursive reference)"

 where
  describe' = Test.Hspec.describe

-- These fixtures are expected to have finite, valid structure.
described :: Agentic m i o -> Description
described = either (error . show) id . Agentic.describe
