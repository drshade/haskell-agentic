-- | A model plays tic-tac-toe against itself: given the game so far, it plays
-- the next move, until it says the game has ended. Needs OPENAI_API_KEY, in the
-- environment or .env.
module Main (main) where

import Agentic
import Agentic.IO (loadDotEnv)
import Agentic.OpenAI (openai)
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

data Square = Blank | X | O
  deriving (Generic, Show, Eq, Contract)

data Row = Row {left :: Square, centre :: Square, right :: Square}
  deriving (Generic, Show, Contract)

data Board = Board {top :: Row, middle :: Row, bottom :: Row}
  deriving (Generic, Show, Contract)

data State = Playing | Ended
  deriving (Generic, Show, Eq, Contract)

data Game = Game {board :: Board, state :: State}
  deriving (Generic, Show, Contract)

-- | One move, played by whichever player's turn it is.
nextMove :: Agentic IO Game Game
nextMove = draft @Game "Play the next move!"

game :: Agentic IO Game Game
game = repeatUntil ((== Ended) . state) (nextMove >>> act printBoard `named` "print the board") `named` "play until the game ends"

printBoard :: Game -> IO Game
printBoard g = T.putStrLn (render (board g) <> "\n") >> pure g

render :: Board -> Text
render (Board t m b) = mconcat [line r <> "\n" | r <- [t, m, b]]
  where
    line (Row l c r) = mconcat [square l, " ", square c, " ", square r]
    square = \case
      Blank -> "."
      X -> "X"
      O -> "O"

main :: IO ()
main = do
  _ <- loadDotEnv
  print $ describe game
  T.putStrLn $ "\n" <> mermaid (describe game)
  T.putStrLn $ dot (describe game)
  rt <- pure runtime >>= withSystemTwo (openai & effort Low)
  let empty = Row Blank Blank Blank
  _ <- interpret rt game (Game (Board empty empty empty) Playing)
  pure ()
