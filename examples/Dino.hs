-- | The dino project from the README. It prints the flow's description, then
-- runs it against mock providers, or for real with @cabal run dino -- live@
-- (Claude and Jev; needs ANTHROPIC_API_KEY and JEV_TOKEN).
module Main (main) where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.IO.DotEnv (loadDotEnv)
import Agentic.Jev (jev)
import Agentic.Scripted (alwaysYes, callTools, replyingWith, respond)
import System.Environment (getArgs)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

data Dino = Dino {name :: Text, facts :: [Text], sources :: [Text]}
  deriving (Generic, Show, Contract)

data DinoPic = DinoPic {asciiPic :: Text}
  deriving (Generic, Show, Contract)

data TrumpCard = TrumpCard {size :: Int, ferocity :: Int, speed :: Int, intelligence :: Int}
  deriving (Generic, Show, Contract)

data Poster = Poster {heading :: Text, body :: Text}
  deriving (Generic, Show, Contract)

dinoProject :: Agentic IO () Poster
dinoProject =
  draft @[Text] "Suggest 3 dinosaurs for a grade 5 project"
    >>> each
      ( ( research
            >>> gate 0.9 kidSafe
            >>> (draft @Dino "Rewrite this for a 10-year-old" ||| returnA)
        )
          &&& draft @DinoPic "Draw an ascii picture of this dinosaur, 10 lines high"
          &&& draft @TrumpCard "Make a trump card for this dinosaur"
      )
    >>> draft @Poster "Create a poster for these dinosaurs"

research :: Agentic IO Text Dino
research = draftWith [fossilSearch, reliable] "Research this dinosaur. Cite a source for every claim."

fossilSearch :: Tool IO
fossilSearch = tool @Text @[Text] "search" "Search the fossil database" (act (\q -> pure ["Fossil record for " <> q]))

reliable :: Tool IO
reliable = tool @Text "is_reliable" "Is this source trustworthy?" (judge (yesNo "Is this a reliable scientific source?"))

kidSafe :: Questions YesNo
kidSafe = yesNo "Is this suitable for a 10-year-old?"

-- | A pretend LLM: it answers according to the type the step asks for.
mockLLM :: Conversation -> Action
mockLLM c = case title (output c) of
  Just "[Text]" -> respond ["Stegosaurus", "Triceratops", "Velociraptor" :: Text]
  Just "Dino"
    | null (history c) -> callTools [("search", state c)]
    | otherwise -> respond (Dino dino ["It lived long ago"] ["Fossil record for " <> dino])
  Just "DinoPic" -> respond (DinoPic ("  /\\_/\\ " <> dino))
  Just "TrumpCard" -> respond (TrumpCard 7 5 6 4)
  Just "Poster" -> respond (Poster "Our Dinosaurs" "Three amazing dinosaurs.")
  _ -> respond ()
  where
    dino = case state c of
      String t -> t
      _ -> "a dinosaur"

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
      else pure runtime {systemOne = alwaysYes 0.95, systemTwo = replyingWith mockLLM}
  let rt =
        observing
          ( \e -> case happened e of
              Drafting c -> T.putStrLn ("  draft " <> maybe "?" id (title (output c)) <> "  " <> T.intercalate " / " (map noteName (path c)))
              ToolCalled call -> T.putStrLn ("    tool call: " <> callName call <> " " <> renderJson (callInput call))
              Judged _ answers -> T.putStrLn ("    judged: " <> T.pack (show answers))
              _ -> pure ()
          )
          providers
  T.putStrLn (if live then "\nRunning it with Claude and Jev:\n" else "\nRunning it against mock providers:\n")
  poster <- interpret rt dinoProject ()
  T.putStrLn ("\n" <> heading poster <> "\n\n" <> body poster)
