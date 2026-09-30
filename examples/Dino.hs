-- | The dino project from the README. Claude suggests prehistoric creatures, Jev
-- sorts the dinosaurs from the rest, and Claude makes a poster of them.
--
-- It prints the flow's description, then runs it with Claude and Jev. Needs
-- ANTHROPIC_API_KEY and JEV_TOKEN, in the environment or .env.
module Main (main) where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.IO (concurrently, loadDotEnv)
import Agentic.Jev (jev)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

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
    >>> arr (partition (clearly Dinosaur 0.8)) `named` "keep the clear dinosaurs (≥ 0.8)"
    >>> (each (arr fst >>> exhibit) *** arr (map notADinosaur))
    >>> arr (uncurry Exhibit)
    >>> draft @Poster "Create a poster of these dinosaurs for a grade 5 class. Add a corner about the creatures that weren't dinosaurs, and what they were."

-- | Jev decides what kind of animal each creature was.
classify :: Agentic IO Creature (Creature, Choice Kind)
classify = returnA &&& judge (choice "What kind of animal was this creature?")

-- | Keep a creature when Jev chose this kind with at least probability @p@.
clearly :: Kind -> Probability -> (Creature, Choice Kind) -> Bool
clearly kind p (_, c) = chosen c == kind && maybe False (>= p) (lookup kind (choiceProbabilities c))

exhibit :: Agentic IO Creature Entry
exhibit =
  (returnA &&& draft @DinoPic "Draw an ascii picture of this dinosaur, 10 lines high" 
           &&& draft @TrumpCard "Make a trump card for this dinosaur")
    `named` "exhibit"
    >>> arr (\(c, (p, t)) -> Entry c p t)

notADinosaur :: (Creature, Choice Kind) -> NotADinosaur
notADinosaur (c, k) = NotADinosaur (name c) (chosen k)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  _ <- loadDotEnv
  print $ describe dinoProject
  rt <- pure (concurrently runtime)
              >>= withSystemOne jev 
              >>= withSystemTwo (anthropic & effort Low)
  poster <- interpret rt dinoProject ()
  T.putStrLn $ "\n" <> heading poster <> "\n\n" <> body poster
