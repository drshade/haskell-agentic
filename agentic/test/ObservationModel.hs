-- A deliberately test-only consumer: graph + recorded metadata, no interpreter.
module ObservationModel (replay, closeRecording, openItems, State (..), Status (..), prefixesValid, OperationOrigin (..), operationOrigins) where

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
  terminal = \case Returned _ -> True; Failed -> True; Cancelled -> True; Suspended -> True; _ -> False

prefixesValid :: FlowGraph -> [Summary] -> Bool
prefixesValid graph = all valid . inits
 where
  valid events = either (const False) (const True) (replay graph events)
    && either (const False) (const True) (operationOrigins events)

-- A recording is one attempt. Start a fresh fold for another attempt: repeating
-- an occurrence there is a retry, not duplicate delivery in this attempt.
data OperationOrigin = Live | FromJournal deriving (Eq, Show)

-- Missing rows are deliberately unclassified. In particular, Entered and
-- Drafting do not invent an origin or tell a viewer to animate new work.
operationOrigins :: [Summary] -> Either String [(Occurrence,LiveOperation,OperationOrigin)]
operationOrigins = foldl advance (Right [])
 where
  advance prior e = do
    rows <- prior
    case e.happened of
      Executing (KeyedOperation AwaitedReply) -> Left "await miss cannot execute"
      Executing op -> insert rows e op Live
      Restored op -> insert rows e (KeyedOperation op) FromJournal
      Returned _ | e.nodeType `elem` [EffectNode,CallNode,AwaitNode,DraftNode]
        , not (any (\(key,_,_) -> key == e.occurrence) rows) ->
          Left "operation returned without classification"
      Turned n _ | not (any (\(key,op,_) -> key == e.occurrence
                      && op == KeyedOperation (DraftTurn n)) rows) ->
        Left "turned without operation classification"
      _ -> Right rows
  insert rows e op origin
    | expectedNode op /= e.nodeType = Left "operation has wrong node kind"
    | any (\(key,old,_) -> key == e.occurrence && (old == op || e.nodeType == EffectNode)) rows =
        Left "operation classified twice"
    | otherwise = Right (rows <> [(e.occurrence,op,origin)])
  expectedNode = \case
    PlainAction -> EffectNode
    KeyedOperation DurableEffect -> EffectNode
    KeyedOperation (DraftTurn _) -> DraftNode
    KeyedOperation Judgement -> JudgeNode
    KeyedOperation ToolExecution -> CallNode
    KeyedOperation AwaitedReply -> AwaitNode
