-- | The dino project from the README. Claude suggests prehistoric creatures, Jev
-- sorts the dinosaurs from the rest, and Claude makes a poster of them.
--
-- It prints the flow's description, then runs it against mock providers, or
-- for real with @cabal run dino -- live@ (Claude and Jev; needs
-- ANTHROPIC_API_KEY and JEV_TOKEN).
module Main (main) where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.IO.DotEnv (loadDotEnv)
import Agentic.Jev (jev)
import Agentic.Scripted (replyingWith, respond)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.Environment (getArgs)

-- ---------------------------------------------------------------------------
-- Types

data Creature = Creature {name :: Text, about :: Text}
  deriving (Generic, Show)

instance Contract Creature where
  contract =
    record "A prehistoric creature" $
      Creature
        <$> required "name" "Its common name, e.g. Triceratops" name
        <*> required "about" "One sentence on what it was and how it lived" about

-- | What kind of animal a creature was. Jev chooses one, with probabilities.
data Kind = Dinosaur | Pterosaur | MarineReptile | Fish | Mammal | Bird | Other
  deriving (Generic, Show, Eq)

instance Options Kind where
  options =
    described
      "What kind of animal it was"
      [ option Dinosaur "A dinosaur that isn't a bird, such as Triceratops or Velociraptor"
      , option Pterosaur "A flying reptile, such as Pteranodon. Not a dinosaur."
      , option MarineReptile "A swimming reptile, such as a plesiosaur, ichthyosaur or mosasaur. Not a dinosaur."
      , option Fish "A fish, such as Dunkleosteus or a prehistoric shark"
      , option Mammal "A mammal, such as a woolly mammoth or sabre-toothed cat"
      , option Bird "A bird, including early birds such as Archaeopteryx"
      , option Other "Anything else: insects, amphibians, crocodile relatives and so on"
      ]

deriving via Enumeration Kind instance Contract Kind

-- | A trump card stat. The contract tells the model the scale and checks it.
newtype Stat = Stat Int
  deriving (Show)

instance Contract Stat where
  contract = documented "From 1 (lowest) to 10 (highest)" (mapCodec Stat (\(Stat n) -> n) (between 1 10 contract))

data TrumpCard = TrumpCard {size :: Stat, ferocity :: Stat, speed :: Stat, intelligence :: Stat}
  deriving (Generic, Show, Contract)

data DinoPic = DinoPic {asciiPic :: Text}
  deriving (Generic, Show, Contract)

data Entry = Entry {dinosaur :: Creature, picture :: DinoPic, card :: TrumpCard}
  deriving (Generic, Show, Contract)

data NotADinosaur = NotADinosaur {creature :: Text, actually :: Kind}
  deriving (Generic, Show, Contract)

data Exhibit = Exhibit {dinosaurs :: [Entry], notDinosaurs :: [NotADinosaur]}
  deriving (Generic, Show, Contract)

data Poster = Poster {heading :: Text, body :: Text}
  deriving (Generic, Show, Contract)

-- ---------------------------------------------------------------------------
-- The flow

dinoProject :: Agentic IO () Poster
dinoProject =
  draft @[Creature] "Name 10 prehistoric creatures a grade 5 class might have heard of. Include a mix of kinds, not only dinosaurs."
    >>> each classify
    >>> arr (partition (clearly Dinosaur 0.8))
    >>> (each (arr fst >>> exhibit) *** arr (map notADinosaur))
    >>> arr (uncurry Exhibit)
    >>> draft @Poster "Create a poster of these dinosaurs for a grade 5 class. Add a corner about the creatures that weren't dinosaurs, and what they were."

-- | Jev decides what kind of animal each creature was.
classify :: Agentic IO Creature (Creature, Choice Kind)
classify = returnA &&& judge (choice "What kind of animal was this creature?") <?> "classify"

-- | Keep a creature when Jev chose this kind with at least probability @p@.
clearly :: Kind -> Probability -> (Creature, Choice Kind) -> Bool
clearly kind p (_, c) = chosen c == kind && maybe False (>= p) (lookup kind (choiceProbabilities c))

exhibit :: Agentic IO Creature Entry
exhibit =
  (returnA &&& draft @DinoPic "Draw an ascii picture of this dinosaur, 10 lines high" &&& draft @TrumpCard "Make a trump card for this dinosaur")
    >>> arr (\(c, (p, t)) -> Entry c p t)
    <?> "exhibit"

notADinosaur :: (Creature, Choice Kind) -> NotADinosaur
notADinosaur (c, k) = NotADinosaur (name c) (chosen k)

-- ---------------------------------------------------------------------------
-- Mock providers

-- | A pretend LLM: it answers according to the type the step asks for.
mockLLM :: Conversation -> Action
mockLLM c = case title (output c) of
  Just "[Creature]" ->
    respond
      [ Creature "Tyrannosaurus rex" "A huge meat-eating dinosaur."
      , Creature "Pteranodon" "A flying reptile with a long crest."
      , Creature "Triceratops" "A plant-eating dinosaur with three horns."
      , Creature "Plesiosaurus" "A long-necked reptile that swam in the sea."
      , Creature "Woolly mammoth" "A hairy relative of the elephant."
      ]
  Just "DinoPic" -> respond (DinoPic "  /\\_/\\  roar")
  Just "TrumpCard" -> respond (TrumpCard (Stat 7) (Stat 5) (Stat 6) (Stat 4))
  Just "Poster" -> respond (Poster "Our Dinosaurs" "Two dinosaurs, and three creatures that weren't.")
  _ -> respond ()

-- | A pretend Jev: it knows which of the mock creatures are dinosaurs.
mockJev :: SystemOne IO
mockJev = SystemOne $ \request ->
  let text = renderJson (requestState request)
      kind
        | any (`T.isInfixOf` text) ["Tyrannosaurus", "Triceratops"] = "Dinosaur"
        | "Pteranodon" `T.isInfixOf` text = "Pterosaur"
        | "Plesiosaurus" `T.isInfixOf` text = "MarineReptile"
        | otherwise = "Mammal"
      labels = ["Dinosaur", "Pterosaur", "MarineReptile", "Fish", "Mammal", "Bird", "Other"]
   in pure [ChoiceAnswer kind [(l, if l == kind then 0.9 else 0.1 / 6) | l <- labels] 0.8]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  live <- (== ["live"]) <$> getArgs
  T.putStrLn "The flow:\n"
  print (describe dinoProject)
  providers <-
    if live
      then do
        _ <- loadDotEnv
        pure runtime >>= withSystemOne jev >>= withSystemTwo (anthropic & effort Low)
      else pure runtime {systemOne = mockJev, systemTwo = replyingWith mockLLM}
  let rt =
        observing
          ( \e -> case happened e of
              Judged request [ChoiceAnswer kind ps _] ->
                T.putStrLn ("  " <> creatureName (requestState request) <> ": " <> kind <> " (" <> T.pack (show (maybe 0 probability (lookup kind ps))) <> ")")
              _ -> pure ()
          )
          providers
  T.putStrLn (if live then "\nRunning it with Claude and Jev:\n" else "\nRunning it against mock providers:\n")
  poster <- interpret rt dinoProject ()
  T.putStrLn ("\n" <> heading poster <> "\n\n" <> body poster)
  where
    creatureName = \case
      Object kvs | Just (String n) <- lookup "name" kvs -> n
      _ -> "?"
