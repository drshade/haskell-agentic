-- | Claude plays tic-tac-toe against code, using tools. The board lives in an
-- IORef; Claude's only tools are @look@ and @play@. The draft step can't end
-- until Claude reports an 'Outcome', and a final @act@ checks that claim
-- against the real board. Needs ANTHROPIC_API_KEY, in the environment or .env.
module Main (main) where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.IO (loadDotEnv)
import Data.IORef
import Data.List (find, transpose)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- The game

data Player = X | O
  deriving (Eq, Show)

-- | Rows of squares, top to bottom.
type Board = [[Maybe Player]]

empty :: Board
empty = replicate 3 (replicate 3 Nothing)

winner :: Board -> Maybe Player
winner b = find (\p -> any (all (== Just p)) lines') [X, O]
  where
    lines' = b <> transpose b <> [[b !! i !! i | i <- [0 .. 2]], [b !! i !! (2 - i) | i <- [0 .. 2]]]

full :: Board -> Bool
full = all (all isJust)

place :: Player -> (Int, Int) -> Board -> Board
place p (r, c) b = [[if (i, j) == (r, c) then Just p else sq | (j, sq) <- zip [0 ..] row] | (i, row) <- zip [0 ..] b]

free :: Board -> [(Int, Int)]
free b = [(i, j) | (i, row) <- zip [0 ..] b, (j, sq) <- zip [0 ..] row, isNothing sq]

-- | The opponent: win if it can, block if it must, otherwise the first free
-- square. Nothing when the board is full.
opponent :: Board -> Maybe (Int, Int)
opponent b = listToMaybe (wins O <> wins X <> free b)
  where
    wins p = [sq | sq <- free b, winner (place p sq b) == Just p]

render :: Board -> Text
render b = T.intercalate "\n" [T.intercalate " " [maybe "." (T.pack . show) sq | sq <- row] | row <- b]

-- ---------------------------------------------------------------------------
-- What Claude sees

-- | A square, from 1 to 3. The contract tells the model the range and checks it.
newtype Coordinate = Coordinate Int
  deriving (Show)

instance Contract Coordinate where
  contract = documented "From 1 to 3" (mapCodec Coordinate (\(Coordinate n) -> n) (between 1 3 contract))

data Move = Move {row :: Coordinate, column :: Coordinate}
  deriving (Generic, Show)

instance Contract Move where
  contract =
    record "Where to place your X" $
      Move
        <$> required "row" "1 is the top row, 3 the bottom" row
        <*> required "column" "1 is the left column, 3 the right" column

data Outcome = Won | Lost | Draw
  deriving (Generic, Show, Eq)

instance Options Outcome where
  options =
    described
      "How the game ended for you, playing X"
      [option Won "You got three in a row", option Lost "O got three in a row", option Draw "The board filled up with no winner"]

deriving via Enumeration Outcome instance Contract Outcome

-- ---------------------------------------------------------------------------
-- The flow

game :: IORef Board -> Agentic IO () (Outcome, Outcome)
game board =
  draftWith @Outcome
    [look board, play board]
    "You are X in a game of tic-tac-toe against O, and you move first. Look at the board, then play one move at a time until the game is over. Then report how it ended."
    >>> (returnA &&& act (const (actual <$> readIORef board))) `named` "check the claim"

look :: IORef Board -> Tool IO
look board =
  tool @() @Text "look" "Show the board: rows top to bottom, X and O for pieces, . for empty squares" $
    act (const (render <$> readIORef board))

-- | Place an X, then let O reply. Illegal moves are reported, not raised.
play :: IORef Board -> Tool IO
play board = tool @Move @Text "play" "Place an X on an empty square. O replies straight away." $ act $ \(Move (Coordinate r) (Coordinate c)) -> do
  b <- readIORef board
  let square = (r - 1, c - 1)
  if isJust (winner b) || full b
    then pure "The game is already over."
    else
      if square `notElem` free b
        then pure ("That square is taken. The board is:\n" <> render b)
        else do
          let afterX = place X square b
              afterO = if isJust (winner afterX) then afterX else maybe afterX (\sq -> place O sq afterX) (opponent afterX)
          writeIORef board afterO
          pure (render afterO <> "\n" <> status afterO)
  where
    status b = case (winner b, full b) of
      (Just X, _) -> "Game over: you won."
      (Just O, _) -> "Game over: O won."
      (Nothing, True) -> "Game over: a draw."
      _ -> "Your move."

actual :: Board -> Outcome
actual b = case winner b of
  Just X -> Won
  Just O -> Lost
  Nothing -> Draw

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  _ <- loadDotEnv
  board <- newIORef empty
  print $ describe (game board)
  rt <-
    pure runtime >>= withSystemTwo (anthropic & effort Low)
  let watched =
        observing
          ( \e -> case happened e of
              ToolCalled call -> T.putStrLn ("\n" <> callName call <> " " <> renderJson (callInput call))
              ToolReturned _ (ToolOk (String t)) -> T.putStrLn t
              _ -> pure ()
          )
          rt
  (claimed, real) <- interpret watched (game board) ()
  T.putStrLn ("\nClaude says: " <> T.pack (show claimed) <> ". The board says: " <> T.pack (show real) <> ".")
