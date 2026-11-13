-- | Looking at a flow without running it.
module Agentic.Describe
  ( describe
  , Description (..)
  , StepInfo (..)
  , ToolInfo (..)
  , ToolBody (..)
  , Address
  , addressText
  , renderTree
  , mermaid
  , dot
  , flowGraph
  , FlowGraph (..)
  , Item (..)
  , NodeKind (..)
  , Edge (..)
  , EdgeStyle (..)
  , descriptionValue
  ) where

import Agentic.Contract (Outline, outline, outlineLabel)
import Agentic.Core
import Agentic.Internal.Static qualified as S
import Agentic.Internal.Static (Address, addressText)
import Agentic.Runtime (FlowError)
import Agentic.Questions (QuestionSpec (..), Questions (..))
import Agentic.Value (Value (..), renderJson)
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
  | FirstHalf
    -- ^ 'takeFirst'.
  | SecondHalf
    -- ^ 'takeSecond'.
  | Effect
    -- ^ @act@: plain code with an effect.
  | DraftInfo
      { instruction :: Instruction
      , input :: Outline
      , output :: Outline
      , tools :: [ToolInfo]
      }
  | JudgeInfo
      { input :: Outline
      , questions :: [QuestionSpec]
      }

data ToolInfo = ToolInfo
  { address :: Address
  , name :: Text
  , description :: Text
  , input :: Outline
  , output :: Outline
  , body :: ToolBody
  }

-- | A finite tool body, a group's definitions, or an explicit reference to
-- a member's structural address. Addresses identify positions in this captured
-- description only; they are not identities across revisions.
data ToolBody
  = ToolFlow Description
  | ToolGroup Int [ToolInfo]
    -- ^ The selected entry member (zero based), and every group definition.
  | ToolReference Address

-- | Describe a flow without running effects or compiling model contracts.
-- Invalid recursive definitions and tool-body nesting beyond 100 are errors.
describe :: Agentic m i o -> Either FlowError Description
describe = fmap fromStatic . S.staticFlow

fromStatic :: S.StaticFlow -> Description
fromStatic = \case
  S.SStep s -> Leaf (stepInfo s)
  S.SSeq f g -> Sequence (sequenced (fromStatic f) <> sequenced (fromStatic g))
  S.SFanout f g -> Together (fromStatic f : together (fromStatic g))
  S.SSplit f g -> Halves (fromStatic f) (fromStatic g)
  S.SFirst f -> Halves (fromStatic f) (Leaf Identity)
  S.SChoose f g -> Branch (fromStatic f) (fromStatic g)
  S.SEach f -> ForEach (fromStatic f)
  S.SRepeat f -> Repeated (fromStatic f)
  S.SNoted n f -> Annotated n (fromStatic f)
 where
  sequenced = \case
    Sequence ds -> ds
    d -> [d]
  together = \case
    Together ds -> ds
    d -> [d]

stepInfo :: S.StaticStep -> StepInfo
stepInfo = \case
  S.SIdentity -> Identity
  S.SGlue -> Glue
  S.SFirstHalf -> FirstHalf
  S.SSecondHalf -> SecondHalf
  S.SEffect -> Effect
  S.SDraft input out instruction tools -> DraftInfo instruction (outline input) (outline out) (map toolInfo tools)
  S.SJudge input qs -> JudgeInfo (outline input) qs.specs

toolInfo :: S.StaticTool -> ToolInfo
toolInfo (S.StaticTool address name description input out body) =
  ToolInfo address name description (outline input) (outline out) $ case body of
    S.Body f -> ToolFlow (fromStatic f)
    S.Members selected members -> ToolGroup selected (map toolInfo members)
    S.ReferenceTo target -> ToolReference target

-- ---------------------------------------------------------------------------
-- The tree view

instance Show Description where
  show = T.unpack . renderTree

data Tree = Node Text [Tree]

-- | The tree view. Recursive groups list their finite definitions; references
-- point to members without expanding them. Unnamed glue between steps is
-- hidden, but a branch is never hidden: inside @&&&@, @***@ and @|||@ it shows
-- as @arr@ (or @pass@), and a pass-through beside a step keeps its input.
renderTree :: Description -> Text
renderTree = T.intercalate "\n" . concatMap (draw "" "") . trees

trees :: Description -> [Tree]
trees = \case
  Leaf Identity -> []
  Leaf Glue -> []
  Leaf FirstHalf -> []
  Leaf SecondHalf -> []
  Leaf info -> leaf Nothing info
  Sequence ds -> concatMap trees ds
  Together ds ->
    let keeping = if any passes ds then "  (keeping its input)" else ""
     in case concatMap parallel (filter (not . passes) ds) of
          [Node t cs] -> [Node (t <> keeping) cs]
          ts -> [Node ("together" <> keeping) ts]
  Halves l r -> [Node "both halves" [labelled "first" (branch l), labelled "second" (branch r)]]
  Branch l r -> [Node "branch" [labelled "left" (branch l), labelled "right" (branch r)]]
  Repeated d -> loop "repeatUntil" d
  ForEach d -> loop "each" d
  Annotated n (Leaf info) | not (passes (Leaf info)) -> leaf (Just n.name) info
  Annotated n d -> case trees d of
    [Node t cs] -> [Node (n.name <> "  " <> t) cs]
    [] -> [Node n.name []]
    ts -> [Node n.name ts]
 where
  leaf name info =
    let toolTrees = case info of
          DraftInfo _ _ _ ts -> map toolTree ts
          _ -> []
     in [Node (T.intercalate "  " (stepLines name info)) toolTrees]
  -- Group a multi-step parallel branch so its steps don't read as branches.
  parallel d = case branch d of
    ts@(_ : _ : _) -> [Node "in order" ts]
    ts -> ts
  branch d = case trees d of
    [] -> [Node (plumbing d) []]
    ts -> ts
  loop label d = case branch d of
    [Node "together" ts] -> [Node label ts]
    ts -> [Node label ts]
  toolTree t = case t.body of
    ToolReference _ -> Node ("tool " <> t.name <> "  (recursive reference)") []
    ToolGroup _ members -> Node ("recursive tools  (entry " <> t.name <> ")") (map toolTree members)
    ToolFlow body -> case trees body of
      [Node label cs] -> Node ("tool " <> t.name <> "  " <> label) cs
      ts -> Node ("tool " <> t.name) ts
  labelled l = \case
    [Node t cs] -> Node (l <> " → " <> t) cs
    [] -> Node (l <> " → pass") []
    ts -> Node l ts

-- | How a branch that's only plumbing shows in the tree view.
plumbing :: Description -> Text
plumbing = \case
  d | passes d -> "pass"
  Leaf FirstHalf -> "takeFirst"
  Leaf SecondHalf -> "takeSecond"
  Annotated _ d -> plumbing d
  _ -> "arr"

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
  { items :: [Item]
  , edges :: [Edge]
  }

-- | A node, or a box of items.
data Item
  = ItemNode Text NodeKind [Text]
    -- ^ An id, what kind of node it is, and its label's lines.
  | ItemBox Text [Text] [Item]
    -- ^ An id, its label's lines, and what's inside.

data NodeKind = Terminal | StepNode | ToolNode

data Edge = Edge
  { from :: Text
  , to :: Text
    -- ^ A node, or a box's id.
  , label :: Maybe Text
  , style :: EdgeStyle
  }

data EdgeStyle = Flow | Uses | Again

-- | The flow's graph, from @input@ to @output@.
flowGraph :: Description -> FlowGraph
flowGraph d = case flow.runBuild (BuildState 0 [[]] []) of
  (_, BuildState _ open edges) -> FlowGraph (reverse (concat open)) (reverse edges)
  where
    flow = do
      item (ItemNode "input" Terminal ["input"])
      exits <- build InSequence (Sources [("input", Nothing)]) d
      item (ItemNode "output" Terminal ["output"])
      connect exits "output"

-- | Where a description sits: unnamed glue between steps is plumbing, but a
-- branch that's only glue is still a branch.
data Context = InSequence | InBranch

-- | Where the next step's input comes from: nodes, each with an optional edge
-- label, or a pair (after @&&&@ or @***@) whose halves are known separately, so
-- a later @first@, @second@ or @***@ wires each half to its own flow.
data From = Sources [(Text, Maybe Text)] | Pair From From

-- | Every node a 'From' draws on, once each. A node reached by routes with
-- different labels (both sides of a @|||@) keeps one unlabelled edge.
sources :: From -> [(Text, Maybe Text)]
sources = dedupe . go
  where
    go = \case
      Sources s -> s
      Pair a b -> go a <> go b
    dedupe = \case
      [] -> []
      (n, l) : rest ->
        let others = [l' | (n', l') <- rest, n' == n]
         in (n, if all (== l) others then l else Nothing) : dedupe [x | x@(n', _) <- rest, n' /= n]

-- | The two sides of a @|||@ joining again: where both made a pair, so does
-- the join.
merge :: From -> From -> From
merge (Pair a b) (Pair c d) = Pair (merge a c) (merge b d)
merge x y = Sources (sources (Pair x y))

-- | A counter for ids, the items of each open box (innermost first), and edges.
data BuildState = BuildState Int [[Item]] [Edge]

newtype Build a = Build {runBuild :: BuildState -> (a, BuildState)}

instance Functor Build where
  fmap f (Build g) = Build (\s -> let (a, s') = g s in (f a, s'))

instance Applicative Build where
  pure a = Build (\s -> (a, s))
  Build f <*> Build g = Build (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))

instance Monad Build where
  Build g >>= k = Build (\s -> let (a, s1) = g s in (k a).runBuild s1)

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
   in (nubOrdered [e.to | e <- new, e.from `elem` sources], s)
  where
    nubOrdered = foldr (\x acc -> x : filter (/= x) acc) []

connect :: From -> Text -> Build ()
connect from to = mapM_ (\(f, l) -> edge (Edge f to l Flow)) (sources from)

node :: From -> [Text] -> Build From
node from label = do
  n <- fresh
  item (ItemNode n StepNode label)
  connect from n
  pure (Sources [(n, Nothing)])

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
  -- A known pair's half goes on exactly; otherwise it's like any @arr@.
  Leaf FirstHalf | Pair a _ <- from -> pure a
  Leaf SecondHalf | Pair _ b <- from -> pure b
  Leaf FirstHalf -> half "takeFirst"
  Leaf SecondHalf -> half "takeSecond"
  Leaf Glue -> case context of
    -- An @arr@ can take a pair apart any way it likes, so its halves are lost,
    -- and so is what the edge labels inside it said.
    InSequence -> pure $ case from of
      Pair _ _ -> Sources [(n, Nothing) | (n, _) <- sources from]
      Sources _ -> from
    InBranch -> node from ["arr"]
  Leaf info -> step Nothing info
  Sequence ds -> chain from ds
  Together ds -> pairUp <$> mapM (build InBranch from) ds
  -- A pair whose halves are known sends each to its flow. Otherwise every
  -- source might hold either half, so the edges say which half they carry.
  Halves l r -> case from of
    Pair a b -> Pair <$> build InBranch a l <*> build InBranch b r
    _ -> Pair <$> build InBranch (labelled "first") l <*> build InBranch (labelled "second") r
  Branch l r -> merge <$> build InBranch (labelled "left") l <*> build InBranch (labelled "right") r
  ForEach f -> Sources . sources . snd <$> box ["each"] (build InSequence (Sources (sources from)) f)
  -- "Again" goes back to where the body starts: the steps the loop's input
  -- flows into. If the body has none, it goes to the box.
  Repeated f -> do
    before <- edgeCount
    (b, exits) <- box ["repeatUntil"] (build InSequence from f)
    entries <- entriesSince before (map fst (sources from))
    let targets = if null entries then [b] else entries
    mapM_ (\(e, _) -> mapM_ (\t -> edge (Edge e t (Just "again") Again)) targets) (sources exits)
    -- The condition is checked before the first run, so the input can leave
    -- without going through the body.
    pure (merge from exits)
  Annotated n (Leaf info) | not (passes (Leaf info)) -> step (Just n) info
  Annotated n f -> snd <$> box (n.name : maybe [] pure n.description) (build InSequence from f)
  where
    labelled l = Sources [(f, Just l) | (f, _) <- sources from]
    -- A half of a pair the graph can't follow: plumbing, like @arr@.
    half name = case context of
      InSequence -> pure from
      InBranch -> node from [name]
    pairUp = \case
      [] -> from
      [x] -> x
      x : xs -> Pair x (pairUp xs)
    chain acc = \case
      [] -> pure acc
      x : xs -> build InSequence acc x >>= (`chain` xs)
    -- A step's node, labelled by 'stepLines', with any tools hanging off it.
    -- In a diagram, a named step's description goes under its name.
    step note' info = do
      let lines'' = case (stepLines ((.name) <$> note') info, note' >>= (.description)) of
            (kind : name : details, Just description) -> kind : name : description : details
            (ls, _) -> ls
      exits <- node from lines''
      case info of
        DraftInfo _ _ _ tools ->
          mapM_
            ( \t -> do
                n <- fresh
                item (ItemNode n ToolNode ["tool " <> t.name])
                mapM_ (\(e, _) -> edge (Edge e n Nothing Uses)) (sources exits)
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
      FirstHalf -> ("takeFirst", [])
      SecondHalf -> ("takeSecond", [])
      Effect -> ("act", [])
      DraftInfo instruction _ out _ -> ("draft @" <> outlineLabel out, [quoted instruction.text])
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
    -- JSON's string escapes are also DOT's.
    str = renderJson . String
    inner = T.pack . init . drop 1 . T.unpack . str

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
descriptionValue :: Description -> Value
descriptionValue = \case
  Leaf info -> leaf info
  Sequence ds -> node "sequence" [("steps", Array (map descriptionValue ds))]
  Together ds -> node "together" [("steps", Array (map descriptionValue ds))]
  Halves l r -> node "halves" [("first", descriptionValue l), ("second", descriptionValue r)]
  Branch l r -> node "branch" [("left", descriptionValue l), ("right", descriptionValue r)]
  ForEach d -> node "each" [("step", descriptionValue d)]
  Repeated d -> node "repeat" [("step", descriptionValue d)]
  Annotated n d ->
    node "note" $
      [("name", String n.name)]
        <> maybe [] (\t -> [("description", String t)]) n.description
        <> [("step", descriptionValue d)]
  where
    node kind fields = Object (("kind", String kind) : fields)
    leaf = \case
      Identity -> node "pass" []
      Glue -> node "arr" []
      FirstHalf -> node "takeFirst" []
      SecondHalf -> node "takeSecond" []
      Effect -> node "act" []
      DraftInfo instruction input out tools ->
        node
          "draft"
          [ ("instruction", String instruction.text)
          , ("input", String (outlineLabel input))
          , ("output", String (outlineLabel out))
          , ("tools", Array (map tool tools))
          ]
      JudgeInfo input qs ->
        node "judge" [("input", String (outlineLabel input)), ("questions", Array (map question qs))]
    tool t =
      Object
        [ ("address", String (addressText t.address))
        , ("name", String t.name)
        , ("description", String t.description)
        , ("input", String (outlineLabel t.input))
        , ("output", String (outlineLabel t.output))
        , ("body", case t.body of
            ToolFlow d -> descriptionValue d
            ToolReference target -> Object [("kind", String "reference"), ("target", String (addressText target))]
            ToolGroup selected members -> Object [("kind", String "recursiveTools"), ("entry", Integer (toInteger selected)), ("members", Array (map tool members))])
        ]
    question = \case
      AskYesNo q -> Object [("type", String "yesNo"), ("question", String q)]
      AskChoice q opts -> Object [("type", String "choice"), ("question", String q), ("options", Array [String l | (l, _) <- opts])]
      AskScore q levels -> Object [("type", String "score"), ("question", String q), ("levels", Array [String l | (l, _) <- levels])]
