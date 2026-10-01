-- | Looking at a flow without running it.
module Agentic.Describe
  ( describe
  , Description (..)
  , StepInfo (..)
  , ToolInfo (..)
  , renderTree
  , mermaid
  , dot
  , flowGraph
  , FlowGraph (..)
  , Item (..)
  , NodeKind (..)
  , Edge (..)
  , EdgeStyle (..)
  , toValue
  ) where

import Agentic.Contract (Codec (..))
import Agentic.Core
import Agentic.Questions (QuestionSpec (..), Questions (..))
import Agentic.Schema (Schema, typeLabel)
import Agentic.Value (Value (..))
import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T

data Description
  = Leaf StepInfo
  | Sequence [Description]
    -- ^ @a >>> b >>> c@, flattened.
  | Together [Description]
    -- ^ @a &&& b &&& c@, flattened.
  | Halves Description Description
    -- ^ @a *** b@: one flow on each half of a pair.
  | Branch Description Description
  | ForEach Description
  | Repeated Description
    -- ^ @repeatUntil@: run again on its own output until a condition holds.
  | Annotated Note Description

data StepInfo
  = Identity
    -- ^ The input, unchanged ('returnA').
  | Glue
    -- ^ @arr@: a pure function.
  | Effect
    -- ^ @act@: plain code with an effect.
  | DraftInfo
      { draftInstruction :: Instruction
      , draftInput :: Schema
      , draftOutput :: Schema
      , draftTools :: [ToolInfo]
      }
  | JudgeInfo
      { judgeState :: Schema
      , judgeQuestions :: [QuestionSpec]
      }

data ToolInfo = ToolInfo
  { infoName :: Text
  , infoDescription :: Text
  , infoInput :: Schema
  , infoOutput :: Schema
  , infoBody :: Description
  }

-- | Describe a flow. This never runs anything.
describe :: Agentic m i o -> Description
describe = \case
  Step s -> Leaf (stepInfo s)
  Seq f g -> Sequence (sequenced (describe f) <> sequenced (describe g))
  Fanout f g -> Together (together (describe f) <> together (describe g))
  Split f g -> Halves (describe f) (describe g)
  First f -> Halves (describe f) (Leaf Identity)
  Choose f g -> Branch (describe f) (describe g)
  Each f -> ForEach (describe f)
  Repeat _ f -> Repeated (describe f)
  Noted n f -> Annotated n (describe f)
  where
    sequenced = \case
      Sequence ds -> ds
      d -> [d]
    together = \case
      Together ds -> ds
      d -> [d]

stepInfo :: Step m i o -> StepInfo
stepInfo = \case
  Pass -> Identity
  Wrap _ -> Identity
  Arr _ -> Glue
  Act _ -> Effect
  Draft input out instruction tools ->
    DraftInfo instruction (codecSchema input) (codecSchema out) (map toolInfo tools)
  Judge input qs -> JudgeInfo (codecSchema input) (specs qs)

toolInfo :: Tool m -> ToolInfo
toolInfo (Tool name description input out body) =
  ToolInfo name description (codecSchema input) (codecSchema out) (describe body)

-- ---------------------------------------------------------------------------
-- The tree view

instance Show Description where
  show = T.unpack . renderTree

data Tree = Node Text [Tree]

-- | The tree view. Each tool's body is expanded the first time the tool
-- appears. Unnamed glue between steps is hidden, but a branch is never hidden:
-- inside @&&&@, @***@ and @|||@ it shows as @arr@ (or @pass@ for 'returnA'), and
-- a pass-through beside a step shows as "keeping its input".
renderTree :: Description -> Text
renderTree = T.intercalate "\n" . concatMap (draw "" "") . snd . trees []

-- | Convert to trees, threading the names of tools already expanded.
trees :: [Text] -> Description -> ([Text], [Tree])
trees seen = \case
  Leaf Identity -> (seen, [])
  Leaf Glue -> (seen, [])
  Leaf info -> leaf Nothing info
  Sequence ds -> onSnd concat (mapAccumL trees seen ds)
  Together ds ->
    let keeping = if any passes ds then "  (keeping its input)" else ""
     in case onSnd concat (mapAccumL parallel seen (filter (not . passes) ds)) of
          (seen', [Node t cs]) -> (seen', [Node (t <> keeping) cs])
          (seen', ts) -> (seen', [Node ("together" <> keeping) ts])
  Halves l r ->
    let (seen1, ls) = branch seen l
        (seen2, rs) = branch seen1 r
     in (seen2, [Node "both halves" [labelled "first" ls, labelled "second" rs]])
  Branch l r ->
    let (seen1, ls) = branch seen l
        (seen2, rs) = branch seen1 r
     in (seen2, [Node "branch" [labelled "left" ls, labelled "right" rs]])
  Repeated d -> case branch seen d of
    (seen', [Node "together" ts]) -> (seen', [Node "repeatUntil" ts])
    (seen', ts) -> (seen', [Node "repeatUntil" ts])
  ForEach d -> case branch seen d of
    (seen', [Node "together" ts]) -> (seen', [Node "each" ts])
    (seen', ts) -> (seen', [Node "each" ts])
  Annotated n (Leaf info) | not (passes (Leaf info)) -> leaf (Just (noteName n)) info
  Annotated n d -> case trees seen d of
    (seen', [Node t cs]) -> (seen', [Node (noteName n <> "  " <> t) cs])
    (seen', []) -> (seen', [Node (noteName n) []])
    (seen', ts) -> (seen', [Node (noteName n) ts])
  where
    -- A step: what kind it is, then its name, then the details.
    leaf name info =
      let (seen', toolTrees) = case info of
            DraftInfo _ _ _ tools -> mapAccumL toolTree seen tools
            _ -> (seen, [])
       in (seen', [Node (T.intercalate "  " (stepLines name info)) toolTrees])
    -- A branch of @&&&@ that's several steps in a row is grouped, so its steps
    -- don't read as more parallel branches.
    parallel s d = case branch s d of
      (s', ts@(_ : _ : _)) -> (s', [Node "in order" ts])
      r -> r
    -- A branch always shows, even when it's only glue.
    branch s d = case trees s d of
      (s', []) -> (s', [Node (if passes d then "pass" else "arr") []])
      r -> r
    toolTree s t
      | infoName t `elem` s = (s, Node ("tool " <> infoName t <> "  (see above)") [])
      | otherwise = case trees (infoName t : s) (infoBody t) of
          (s', [Node body cs]) -> (s', Node ("tool " <> infoName t <> "  " <> body) cs)
          (s', ts) -> (s', Node ("tool " <> infoName t) ts)
    labelled l = \case
      [Node t cs] -> Node (l <> " → " <> t) cs
      [] -> Node (l <> " → pass") []
      ts -> Node l ts

-- | Does this part of a flow only pass its input through?
passes :: Description -> Bool
passes = \case
  Leaf Identity -> True
  Sequence ds -> all passes ds
  Annotated _ d -> passes d
  _ -> False

questionText :: QuestionSpec -> Text
questionText = \case
  AskYesNo q -> "yes/no " <> quoted q
  AskChoice q opts -> "choice of " <> T.pack (show (length opts)) <> " " <> quoted q
  AskScore q levels -> "score on " <> T.pack (show (length levels)) <> " levels " <> quoted q

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

draw :: Text -> Text -> Tree -> [Text]
draw lead childLead (Node t cs) = (lead <> t) : go cs
  where
    go = \case
      [] -> []
      [c] -> draw (childLead <> "└─ ") (childLead <> "   ") c
      c : rest -> draw (childLead <> "├─ ") (childLead <> "│  ") c <> go rest

-- ---------------------------------------------------------------------------
-- Mermaid

-- | How data moves through a flow, as a graph: steps joined in order; @&&&@,
-- @***@ and @|||@ forking into their branches and joining again at the next
-- step, with a pass-through drawn as an edge straight to the join; @each@,
-- @repeatUntil@ and named sub-flows as boxes; tools hanging off their draft.
-- 'mermaid' and 'dot' render it.
data FlowGraph = FlowGraph
  { graphItems :: [Item]
  , graphEdges :: [Edge]
  }

-- | A node, or a box of items.
data Item
  = ItemNode Text NodeKind [Text]
    -- ^ An id, what kind of node it is, and its label's lines.
  | ItemBox Text [Text] [Item]
    -- ^ An id, its label's lines, and what's inside.

data NodeKind = Terminal | StepNode | ToolNode

data Edge = Edge
  { edgeFrom :: Text
  , edgeTo :: Text
    -- ^ A node, or a box's id.
  , edgeLabel :: Maybe Text
  , edgeStyle :: EdgeStyle
  }

data EdgeStyle = Flow | Uses | Again

-- | The flow's graph, from @input@ to @output@.
flowGraph :: Description -> FlowGraph
flowGraph d = case runBuild flow (BuildState 0 [[]] []) of
  (_, BuildState _ open edges) -> FlowGraph (reverse (concat open)) (reverse edges)
  where
    flow = do
      item (ItemNode "input" Terminal ["input"])
      exits <- build InSequence [("input", Nothing)] d
      item (ItemNode "output" Terminal ["output"])
      connect exits "output"

-- | Where a description sits: unnamed glue between steps is plumbing, but a
-- branch that's only glue is still a branch.
data Context = InSequence | InBranch

-- | Nodes the next step connects from, each with an optional edge label.
type From = [(Text, Maybe Text)]

-- | A counter for ids, the items of each open box (innermost first), and edges.
data BuildState = BuildState Int [[Item]] [Edge]

newtype Build a = Build {runBuild :: BuildState -> (a, BuildState)}

instance Functor Build where
  fmap f (Build g) = Build (\s -> let (a, s') = g s in (f a, s'))

instance Applicative Build where
  pure a = Build (\s -> (a, s))
  Build f <*> Build g = Build (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))

instance Monad Build where
  Build g >>= k = Build (\s -> let (a, s1) = g s in runBuild (k a) s1)

fresh :: Build Text
fresh = Build (\(BuildState n open es) -> ("n" <> T.pack (show n), BuildState (n + 1) open es))

item :: Item -> Build ()
item i = Build $ \case
  BuildState n (current : outer) es -> ((), BuildState n ((i : current) : outer) es)
  BuildState n [] es -> ((), BuildState n [[i]] es)

edge :: Edge -> Build ()
edge e = Build (\(BuildState n open es) -> ((), BuildState n open (e : es)))

edgeCount :: Build Int
edgeCount = Build (\s@(BuildState _ _ es) -> (length es, s))

-- | The nodes that edges added since @before@ lead into from these sources.
entriesSince :: Int -> [Text] -> Build [Text]
entriesSince before sources = Build $ \s@(BuildState _ _ es) ->
  let new = reverse (take (length es - before) es)
   in (nubOrdered [edgeTo e | e <- new, edgeFrom e `elem` sources], s)
  where
    nubOrdered = foldr (\x acc -> x : filter (/= x) acc) []

connect :: From -> Text -> Build ()
connect from to = mapM_ (\(f, l) -> edge (Edge f to l Flow)) from

node :: From -> [Text] -> Build From
node from label = do
  n <- fresh
  item (ItemNode n StepNode label)
  connect from n
  pure [(n, Nothing)]

box :: [Text] -> Build a -> Build (Text, a)
box label inside = do
  b <- fresh
  Build (\(BuildState n open es) -> ((), BuildState n ([] : open) es))
  a <- inside
  Build $ \case
    BuildState n (contents : parent : outer) es -> ((), BuildState n ((ItemBox b label (reverse contents) : parent) : outer) es)
    s -> ((), s)
  pure (b, a)

build :: Context -> From -> Description -> Build From
build context from = \case
  Leaf Identity -> pure from
  Leaf Glue -> case context of
    InSequence -> pure from
    InBranch -> node from ["arr"]
  Leaf info -> step Nothing info
  Sequence ds -> chain from ds
  Together ds -> concat <$> mapM (build InBranch from) ds
  Halves l r -> (<>) <$> build InBranch (labelled "first") l <*> build InBranch (labelled "second") r
  Branch l r -> (<>) <$> build InBranch (labelled "left") l <*> build InBranch (labelled "right") r
  ForEach f -> snd <$> box ["each"] (build InSequence from f)
  -- "Again" goes back to where the body starts: the steps the loop's input
  -- flows into. If the body has none, it goes to the box.
  Repeated f -> do
    before <- edgeCount
    (b, exits) <- box ["repeatUntil"] (build InSequence from f)
    entries <- entriesSince before (map fst from)
    let targets = if null entries then [b] else entries
    mapM_ (\(e, _) -> mapM_ (\t -> edge (Edge e t (Just "again") Again)) targets) exits
    pure exits
  Annotated n (Leaf info) | not (passes (Leaf info)) -> step (Just n) info
  Annotated n f -> snd <$> box (noteName n : maybe [] pure (noteDescription n)) (build InSequence from f)
  where
    labelled l = [(f, Just l) | (f, _) <- from]
    chain acc = \case
      [] -> pure acc
      x : xs -> build InSequence acc x >>= (`chain` xs)
    -- A step's node, labelled by 'stepLines', with any tools hanging off it.
    -- In a diagram, a named step's description goes under its name.
    step note' info = do
      let lines'' = case (stepLines (noteName <$> note') info, note' >>= noteDescription) of
            (kind : name : details, Just description) -> kind : name : description : details
            (ls, _) -> ls
      exits <- node from lines''
      case info of
        DraftInfo _ _ _ tools ->
          mapM_
            ( \t -> do
                n <- fresh
                item (ItemNode n ToolNode ["tool " <> infoName t])
                mapM_ (\(e, _) -> edge (Edge e n Nothing Uses)) exits
            )
            tools
        _ -> pure ()
      pure exits

-- | How a step is labelled, everywhere: what kind of step it is, then its name
-- if it has one, then its details (an instruction, or questions).
stepLines :: Maybe Text -> StepInfo -> [Text]
stepLines name info = kind : maybe [] pure name <> details
  where
    (kind, details) = case info of
      Identity -> ("pass", [])
      Glue -> ("arr", [])
      Effect -> ("act", [])
      DraftInfo instruction _ out _ -> ("draft @" <> typeLabel out, [quoted (instructionText instruction)])
      JudgeInfo _ [q] -> ("judge", [questionText q])
      JudgeInfo _ qs -> ("judge " <> T.pack (show (length qs)) <> " questions in one request", map questionText qs)

-- | A Mermaid flowchart of the flow's graph.
mermaid :: Description -> Text
mermaid d = T.unlines ("flowchart TD" : concatMap (items' "  ") is <> map edge' es)
  where
    FlowGraph is es = flowGraph d
    items' indent = \case
      ItemNode i kind ls -> [indent <> i <> shape kind (T.intercalate "<br/>" (map escape ls))]
      ItemBox i ls inside -> [indent <> "subgraph " <> i <> "[\"" <> T.intercalate "<br/>" (map escape ls) <> "\"]"] <> concatMap (items' (indent <> "  ")) inside <> [indent <> "end"]
    shape kind l = case kind of
      Terminal -> "([\"" <> l <> "\"])"
      StepNode -> "[\"" <> l <> "\"]"
      ToolNode -> "[/\"" <> l <> "\"/]"
    edge' (Edge f t l style) = "  " <> f <> arrow style <> maybe "" (\x -> "|" <> escape x <> "|") l <> " " <> t
    arrow = \case
      Flow -> " -->"
      Uses -> " -.-"
      Again -> " -.->"
    escape = T.replace "\"" "#quot;"

-- | A Graphviz DOT digraph of the flow's graph. Render it with, for example,
-- @dot -Tsvg@.
dot :: Description -> Text
dot d =
  T.unlines $
    ["digraph flow {", "  compound=true;", "  node [shape=box, style=rounded, fontname=\"Helvetica\"];", "  edge [fontname=\"Helvetica\"];"]
      <> concatMap (items' "  ") is
      <> map edge' es
      <> ["}"]
  where
    FlowGraph is es = flowGraph d
    items' indent = \case
      ItemNode i kind ls -> [indent <> i <> " [label=\"" <> T.intercalate "\\n" (map inner ls) <> "\"" <> shape kind <> "];"]
      ItemBox i ls inside -> [indent <> "subgraph cluster_" <> i <> " {", indent <> "  label=\"" <> T.intercalate "\\n" (map inner ls) <> "\";", indent <> "  style=rounded;"] <> concatMap (items' (indent <> "  ")) inside <> [indent <> "}"]
    shape = \case
      Terminal -> ", shape=oval"
      StepNode -> ""
      ToolNode -> ", shape=parallelogram, style=\"\""
    -- An edge into a box points at the box's first node, clipped to the box.
    edge' (Edge f t l style) =
      let (target, attrs) = case firstNode t is of
            Just n
              | inBox t f is -> (n, [])
              | otherwise -> (n, ["lhead=cluster_" <> t])
            Nothing -> (t, [])
          extra = maybe [] (\x -> ["label=" <> str x]) l <> attrs <> styleOf style
       in "  " <> f <> " -> " <> target <> (if null extra then "" else " [" <> T.intercalate ", " extra <> "]") <> ";"
    styleOf = \case
      Flow -> []
      Uses -> ["style=dotted", "arrowhead=none"]
      Again -> ["style=dashed"]
    str t = "\"" <> inner t <> "\""
    inner = concatMapText (\case '"' -> "\\\""; '\\' -> "\\\\"; c -> T.singleton c)

-- | Is the node with this id inside the box with that id?
inBox :: Text -> Text -> [Item] -> Bool
inBox b n = any within
  where
    within = \case
      ItemBox i _ inside
        | i == b -> any contains inside
        | otherwise -> any within inside
      ItemNode {} -> False
    contains = \case
      ItemNode i _ _ -> i == n
      ItemBox _ _ inside -> any contains inside

-- | The first node inside the box with this id, if the id is a box's. Only an
-- edge into an empty loop needs it.
firstNode :: Text -> [Item] -> Maybe Text
firstNode b = go
  where
    go = \case
      [] -> Nothing
      ItemBox i _ inside : rest
        | i == b -> first inside
        | otherwise -> maybe (go rest) Just (go inside)
      ItemNode {} : rest -> go rest
    first = \case
      ItemNode i _ _ : _ -> Just i
      ItemBox _ _ inside : rest -> maybe (first rest) Just (first inside)
      [] -> Nothing

-- ---------------------------------------------------------------------------
-- JSON

-- | The description as a JSON-shaped value, for UIs and other agents.
toValue :: Description -> Value
toValue = \case
  Leaf info -> leaf info
  Sequence ds -> node "sequence" [("steps", Array (map toValue ds))]
  Together ds -> node "together" [("steps", Array (map toValue ds))]
  Halves l r -> node "halves" [("first", toValue l), ("second", toValue r)]
  Branch l r -> node "branch" [("left", toValue l), ("right", toValue r)]
  ForEach d -> node "each" [("step", toValue d)]
  Repeated d -> node "repeat" [("step", toValue d)]
  Annotated n d ->
    node "note" $
      [("name", String (noteName n))]
        <> maybe [] (\t -> [("description", String t)]) (noteDescription n)
        <> [("step", toValue d)]
  where
    node kind fields = Object (("kind", String kind) : fields)
    leaf = \case
      Identity -> node "pass" []
      Glue -> node "arr" []
      Effect -> node "act" []
      DraftInfo instruction input out tools ->
        node
          "draft"
          [ ("instruction", String (instructionText instruction))
          , ("input", String (typeLabel input))
          , ("output", String (typeLabel out))
          , ("tools", Array (map tool tools))
          ]
      JudgeInfo input qs ->
        node "judge" [("state", String (typeLabel input)), ("questions", Array (map question qs))]
    tool t =
      Object
        [ ("name", String (infoName t))
        , ("description", String (infoDescription t))
        , ("input", String (typeLabel (infoInput t)))
        , ("output", String (typeLabel (infoOutput t)))
        ]
    question = \case
      AskYesNo q -> Object [("type", String "yesNo"), ("question", String q)]
      AskChoice q opts -> Object [("type", String "choice"), ("question", String q), ("options", Array [String l | (l, _) <- opts])]
      AskScore q levels -> Object [("type", String "score"), ("question", String q), ("levels", Array [String l | (l, _) <- levels])]

-- | 'T.concatMap', which MicroHs's "Data.Text" doesn't provide.
concatMapText :: (Char -> Text) -> Text -> Text
concatMapText f = T.concat . map f . T.unpack

-- | Apply a function to a pair's second half. (MicroHs has no Functor instance
-- for pairs.)
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)
