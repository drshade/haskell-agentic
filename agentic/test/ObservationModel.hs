-- A deliberately test-only consumer: graph + recorded metadata, no interpreter.
module ObservationModel (replay, closeRecording, openItems, State (..), Status (..), prefixesValid) where

import Agentic
import Data.List (inits, nub)
import Data.Text (Text)

data Status = Open | Unknown | Finished Happened deriving (Eq, Show)
data State = State [(Occurrence, Maybe Occurrence, NodeType, Status)] deriving (Eq, Show)

-- Derive display state only after tracking individual occurrence lifetimes.
openItems :: FlowGraph -> State -> [(Text,[Occurrence])]
openItems graph (State rows) =
  [(item,[key | (key,_,_,Open) <- rows, lookup key.address graph.addressItems == Just item])
  | item <- nub (map snd graph.addressItems)]

-- Closing a recording is different from temporarily reaching its current end.
closeRecording :: State -> State
closeRecording (State rows) = State [(key,parent,kind,if status == Open then Unknown else status) | (key,parent,kind,status) <- rows]

replay :: FlowGraph -> [Summary] -> Either String State
replay graph = foldl advance (Right (State []))
 where
  advance previous e = do
    State rows <- previous
    let matches = [(p,k,s) | (key,p,k,s) <- rows, key == e.occurrence]
        parentOpen = case e.parent of
          Nothing -> e.nodeType == RunNode
          Just p -> any (\(key,_,_,s) -> key == p && s == Open) rows
        addressKnown = e.nodeType == RunNode || e.occurrence.address `elem` map fst graph.addressItems
    if not addressKnown then Left "unknown address" else case e.happened of
      Entered _
        | not (null matches) -> Left "duplicate entry"
        | not parentOpen -> Left "parent was not open"
        | otherwise -> Right (State (rows <> [(e.occurrence,e.parent,e.nodeType,Open)]))
      phase -> case matches of
        [(p,k,Open)] | p == e.parent && k == e.nodeType ->
          if terminal phase then
            -- Interrupted observer delivery may leave a child open beneath a
            -- terminated ancestor. Keep it unknown; never invent its terminal.
            Right (State [if key == e.occurrence then (key,p,k,Finished phase) else row | row@(key,_,_,_) <- rows])
          else case phase of
            Selected edge | edge `notElem` concatMap (.selections) graph.edges -> Left "unknown selection"
            _ -> Right (State rows)
        _ -> Left "event outside its occurrence or changed parent/kind"
  terminal = \case Returned _ -> True; Failed -> True; Cancelled -> True; _ -> False

prefixesValid :: FlowGraph -> [Summary] -> Bool
prefixesValid graph = all (either (const False) (const True) . replay graph) . inits
