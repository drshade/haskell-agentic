-- | The dino project from the README. Claude suggests prehistoric creatures, Jev
-- sorts the dinosaurs from the rest, and Claude makes a poster of them.
--
-- It prints the flow's description, then runs it with Claude and Jev. Needs
-- ANTHROPIC_API_KEY and JEV_TOKEN, in the environment or .env.
module Dino where

import Agentic
import Control.Exception (throwIO)
import Agentic.Anthropic (anthropic)
import Agentic.IO (concurrently, loadDotEnv)
import Agentic.Jev (jev)
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
        <$> required "name" "Its common name, e.g. Triceratops" (.name)
        <*> required "about" "One sentence on what it was and how it lived" (.about)

-- | What kind of animal a creature was. Jev chooses one, with probabilities.
data Kind = Dinosaur | Pterosaur | MarineReptile | Fish | Mammal | Other
  deriving (Generic, Show, Eq)

instance Options Kind where
  options =
    documentedOptions
      "What kind of animal it was"
      [ option Dinosaur "A dinosaur, including birds, such as Triceratops, Velociraptor or \
        \Archaeopteryx"
      , option Pterosaur "A flying reptile, such as Pteranodon. Not a dinosaur."
      , option MarineReptile "A swimming reptile, such as a plesiosaur, ichthyosaur or mosasaur. \
        \Not a dinosaur."
      , option Fish "A fish, such as Dunkleosteus or a prehistoric shark"
      , option Mammal "A mammal, such as a woolly mammoth or sabre-toothed cat"
      , option Other "Anything else: insects, amphibians, crocodile relatives and so on"
      ]

instance Contract Kind where contract = enumeration

-- | A trump card stat. The contract tells the model the scale and checks it.
newtype Stat = Stat Int
  deriving (Show)

instance Contract Stat where
  contract =
    documented "From 1 (lowest) to 10 (highest)" $
      mapStructure Stat (\(Stat n) -> n) (between 1 10 contract)

data TrumpCard = TrumpCard {size :: Stat, ferocity :: Stat, speed :: Stat, intelligence :: Stat}
  deriving (Generic, Show, Contract)

data DinoPic = DinoPic {asciiPic :: Text}
  deriving (Generic, Show, Contract)

data Entry = Entry {dinosaur :: Creature, picture :: DinoPic, card :: TrumpCard}
  deriving (Generic, Show, Contract)

data NotADinosaur = NotADinosaur {creature :: Creature, actually :: Kind}
  deriving (Generic, Show, Contract)

data FactCard = FactCard {heading :: Text, explanation :: Text}
  deriving (Generic, Show, Contract)

data ExhibitEntry = DinosaurExhibit Entry | OtherExhibit FactCard
  deriving (Generic, Show, Contract)

data Exhibit = Exhibit {entries :: [ExhibitEntry]}
  deriving (Generic, Show, Contract)

data Poster = Poster {heading :: Text, body :: Text}
  deriving (Generic, Show, Contract)

-- ---------------------------------------------------------------------------
-- The flow

dinoProject :: Agentic IO () Poster
dinoProject =
  draft @[Creature] "Name 10 prehistoric creatures a grade 5 class might have heard of. \
    \Include a mix of kinds, not only dinosaurs."
    >>> each creatureExhibit
    >>> arr Exhibit
    >>> draft @Poster "Create a grade 5 poster from these exhibit entries. Keep the dinosaur \
      \pictures and trump cards, and give the other prehistoric creatures a \
      \Not a dinosaur! corner."

-- | Classify once, then run only the chosen kind of exhibit.
creatureExhibit :: Agentic IO Creature ExhibitEntry
creatureExhibit =
  classify
    >>> arr route `named` "dinosaur or other creature"
    >>> ((exhibit >>> arr DinosaurExhibit) ||| (factCard >>> arr OtherExhibit))
          `named` "choose an exhibit"

-- | Jev decides what kind of animal each creature was, keeping the creature.
classify :: Agentic IO Creature (Creature, Choice Kind)
classify = returnA &&& judge (choice "What kind of animal was this creature?")

-- | Follow the chosen kind. Low confidence is not evidence of a different kind.
route :: (Creature, Choice Kind) -> Either Creature NotADinosaur
route (c, k)
  | k.chosen == Dinosaur = Left c
  | otherwise = Right (NotADinosaur c k.chosen)

factCard :: Agentic IO NotADinosaur FactCard
factCard = draft @FactCard "Make a short Not a dinosaur! fact card for a grade 5 class. Name this \
  \creature, explain what kind of animal it was and why it was not a \
  \dinosaur."
  `named` "not a dinosaur fact card"

exhibit :: Agentic IO Creature Entry
exhibit =
  (returnA &&& draft @DinoPic "Draw an ascii picture of this dinosaur, 10 lines high"
           &&& draft @TrumpCard "Make a trump card for this dinosaur")
    `named` "exhibit"
    >>> arr (\(dinosaur :/\ picture :/\ card) -> Entry dinosaur picture card)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  _ <- loadDotEnv
  description <- either throwIO pure (describe dinoProject)
  print description
  T.putStrLn $ "\n" <> mermaid description
  T.putStrLn $ dot description
  rt <- pure (concurrently runtime)
              >>= withSystemOne jev
              >>= withSystemTwo (anthropic & effort Low)
  poster <- interpret rt dinoProject ()
  T.putStrLn $ "\n" <> poster.heading <> "\n\n" <> poster.body
