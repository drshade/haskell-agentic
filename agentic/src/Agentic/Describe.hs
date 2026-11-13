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
  , BodyRelation (..)
  , Item (..)
  , NodeKind (..)
  , Edge (..)
  , EdgeStyle (..)
  , descriptionValue
  , flowGraphValue
  ) where

import Agentic.Contract (Outline, outline, outlineLabel)
import Agentic.Core
import Agentic.Internal.Static qualified as S
import Agentic.Internal.Address
import Agentic.Runtime (FlowError, EdgeId (..), Side (..), edgeIdText)
import Agentic.Questions (QuestionSpec (..), Questions (..))
import Agentic.Value (Value (..), renderJson)
import Data.List (find, isPrefixOf, maximumBy, nub)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

data Description
  = Located Address Description
  | FirstOnly Description
  | Leaf StepInfo
  | Sequence [Description]
    -- ^ Composition, retaining structural addresses.
  | Together [Description]
    -- ^ Parallel composition, retaining structural addresses.
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
fromStatic flow = Located flow.address $ case flow.shape of
  S.SStep s -> Leaf (stepInfo s)
  S.SSeq f g -> Sequence [fromStatic f, fromStatic g]
  S.SFanout f g -> Together [fromStatic f, fromStatic g]
  S.SSplit f g -> Halves (fromStatic f) (fromStatic g)
  S.SFirst f -> FirstOnly (fromStatic f)
  S.SChoose f g -> Branch (fromStatic f) (fromStatic g)
  S.SEach f -> ForEach (fromStatic f)
  S.SRepeat f -> Repeated (fromStatic f)
  S.SNoted n f -> Annotated n (fromStatic f)

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
renderTree = T.intercalate "\n" . concatMap (draw "" "") . trees . displayTree

-- Flatten only the human tree; the captured description retains every address.
displayTree :: Description -> Description
displayTree = \case
  Located _ d -> displayTree d
  FirstOnly d -> Halves (displayTree d) (Leaf Identity)
  Sequence ds -> Sequence (concatMap (flatten . displayTree) ds)
    where flatten (Sequence xs) = xs; flatten d = [d]
  Together ds -> Together (concatMap (flatten . displayTree) ds)
    where flatten (Together xs) = xs; flatten d = [d]
  Halves l r -> Halves (displayTree l) (displayTree r)
  Branch l r -> Branch (displayTree l) (displayTree r)
  ForEach d -> ForEach (displayTree d)
  Repeated d -> Repeated (displayTree d)
  Annotated n d -> Annotated n (displayTree d)
  d -> d

trees :: Description -> [Tree]
trees = \case
  Located _ d -> trees (displayTree d)
  FirstOnly d -> trees (displayTree (FirstOnly d))
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
  Located _ d -> plumbing d
  d | passes d -> "pass"
  Leaf FirstHalf -> "takeFirst"
  Leaf SecondHalf -> "takeSecond"
  Annotated _ d -> plumbing d
  _ -> "arr"

-- | Does this part of a flow only pass its input through?
passes :: Description -> Bool
passes = \case
  Located _ d -> passes d
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
  , addressItems :: [(Address, Text)]
    -- ^ Every captured address mapped to an item, including hidden aliases.
    -- The logical root is the fallback; Mermaid and DOT do not draw its box.
  , bodies :: [BodyRelation]
  }

-- | A loop body, or a possible call target with its binding-site address.
data BodyRelation = BodyRelation
  { owner :: Address
  , binding :: Maybe Address
  , bodyRoot :: Address
  } deriving (Eq, Show)

-- | A node, or a box of items.
data Item
  = ItemNode Text NodeKind [Text]
    -- ^ An id, what kind of node it is, and its label's lines.
  | ItemBox Text [Text] [Item]
    -- ^ An id, its label's lines, and what's inside.

data NodeKind = Terminal | StepNode | ToolNode | DecisionNode
  | JunctionNode
    -- ^ Graph-only routing point, with no occurrence/address mapping. A
    -- projected fork can span several syntax positions (including a later
    -- fanout after a pass-through choice), so it does not identify one AST node.
    -- A dot also appears where a selected side feeds both a loop's Again edge
    -- and its exit.
  deriving (Eq, Show)

data Edge = Edge
  { from :: Text
  , to :: Text
    -- ^ A node, or a box's id.
  , label :: Maybe Text
  , style :: EdgeStyle
  , referenceTarget :: Maybe Address
    -- ^ Definition address for a recursive Reference edge; never inferred from its label.
  , selections :: [EdgeId]
    -- ^ A singleton on each decision's two outgoing edges; empty elsewhere.
  } deriving (Eq, Show)

data EdgeStyle = Flow | Uses | Again | Reference
  deriving (Eq, Show)

-- | The flow's graph, from @input@ to @output@.
flowGraph :: Description -> FlowGraph
flowGraph d = FlowGraph finalItems finalEdges mapping relations
 where
  (_, state) = flow.runBuild (BuildState 0 [[]] [] flowAddress [])
  (junctions, finalEdges) = selectionRoutes (reverse state.links)
  finalItems = [ItemBox (itemId rootAddress 0) ["run"] (insertJunctions junctions (reverse (concat state.open)))]
  rawMapping = (rootAddress, itemId rootAddress 0) : state.mapped
  (addresses, relations) = structure d
  mapping = [(a, resolve a) | a <- rootAddress : addresses]
  resolve a@(Address path) = case lookup a rawMapping of
    Just i -> i
    Nothing -> case [(p,i) | (Address p,i) <- rawMapping, p `isPrefixOf` path] of
      [] -> itemId rootAddress 0
      candidates -> snd (maximumBy (comparing (length . fst)) candidates)
  flow = do
    item (ItemNode "input" Terminal ["input"])
    exits <- build InSequence (Sources [("input", Nothing, [])]) d
    item (ItemNode "output" Terminal ["output"])
    connect exits "output"

-- Consolidate a selected side only after downstream wiring is known. A
-- pass-through can fork later, or supply a repeat's carried input. Keeping its
-- side id until here avoids duplicating the decision's labelled edge.
selectionRoutes :: [Edge] -> ([(Text, Item)], [Edge])
selectionRoutes original = shareJunctions (foldl route ([], original) ids)
 where
  ids = nub (concatMap (.selections) original)
  route (items, edges) selection@(EdgeId address side) =
    let selected = [e | e <- edges, selection `elem` e.selections]
        diamond = itemId address 0
        label = case side of LeftSide -> "left"; RightSide -> "right"
        junction = "entry_" <> T.replace "/" "_" (edgeIdText selection)
        isSelected e = selection `elem` e.selections
        redirect e = if isSelected e then
          e {from = junction, label = if e.label == Just label then Nothing else e.label, selections = []}
          else e
     in case selected of
          [e] | e.style == Flow ->
            (items, [if isSelected e then e {label = Just label} else e | e <- edges])
          _ ->
            let entry = Edge diamond junction (Just label) Flow Nothing [selection]
                insert [] = []
                insert (e:rest)
                  | isSelected e = entry : map redirect (e:rest)
                  | otherwise = e : insert rest
             in (items <> [(diamond,ItemNode junction JunctionNode [])], insert edges)

-- Two pass-through sides may reach the same later fork. Share that routing
-- point without merging their two distinct labelled decision edges.
shareJunctions :: ([(Text, Item)], [Edge]) -> ([(Text, Item)], [Edge])
shareJunctions (junctions, edges) = foldl share ([], edges) junctions
 where
  share (kept, edges) entry@(owner, ItemNode current JunctionNode _) =
    let exits node = [(e.to,e.label,e.style,e.referenceTarget,e.selections) | e <- edges, e.from == node]
        same (otherOwner, ItemNode other JunctionNode _) = owner == otherOwner && exits current == exits other
        same _ = False
     in case find same kept of
          Just (_, ItemNode target JunctionNode _) ->
            let redirect e = e {from = if e.from == current then target else e.from,
                                to = if e.to == current then target else e.to}
             in (kept, nub (map redirect edges))
          _ -> (kept <> [entry], edges)
  share (kept, edges) entry = (kept <> [entry], edges)

-- Put routing dots in the same displayed box as their decision.
insertJunctions :: [(Text, Item)] -> [Item] -> [Item]
insertJunctions junctions = concatMap $ \case
  ItemBox i label children -> [ItemBox i label (insertJunctions junctions children)]
  node@(ItemNode i _ _) -> node : [junction | (owner,junction) <- junctions, owner == i]

-- All positions survive the display projection, including synthetic call scopes.
structure :: Description -> ([Address], [BodyRelation])
structure = go flowAddress
 where
  both (as,bs) (cs,ds) = (as <> cs, bs <> ds)
  many = foldr both ([],[]) . map (go flowAddress)
  go a = \case
    Located address d -> let (as,bs) = go address d in (address:as,bs)
    FirstOnly d -> go a d
    Sequence ds -> many ds
    Together ds -> many ds
    Halves l r -> both (go a l) (go a r)
    Branch l r -> both (go a l) (go a r)
    ForEach d -> loop a d
    Repeated d -> loop a d
    Annotated _ d -> go a d
    Leaf (DraftInfo _ _ _ ts) ->
      let call = child a CallScope
          (as,bs) = foldr both ([],[]) (map tool ts)
       in (call:as, [BodyRelation call (Just t.address) (target t) | t <- ts] <> bs)
    Leaf _ -> ([],[])
  loop a d@(Located b _) = let (as,bs) = go a d in (as,BodyRelation a Nothing b:bs)
  loop a d = go a d
  tool t = case t.body of
    ToolFlow d -> go t.address d
    ToolGroup _ ts -> let (as,bs) = foldr both ([],[]) (map tool ts) in (t.address:as,bs)
    ToolReference _ -> ([t.address],[])
  target t = case t.body of
    ToolFlow _ -> t.address
    ToolReference a -> a
    ToolGroup k ts -> target (ts !! k)

-- | Where a description sits: unnamed glue between steps is plumbing, but a
-- branch that's only glue is still a branch.
data Context = InSequence | InBranch

-- | Where the next step's input comes from: nodes, each with an optional edge
-- label, or a pair (after @&&&@ or @***@) whose halves are known separately, so
-- a later @first@, @second@ or @***@ wires each half to its own flow.
data From = Sources [(Text, Maybe Text, [EdgeId])] | Pair From From

-- | Every node a 'From' draws on, once each. A node reached by routes with
-- different labels (both sides of a @|||@) keeps one unlabelled edge.
sources :: From -> [(Text, Maybe Text, [EdgeId])]
sources = dedupe . go
  where
    go = \case
      Sources s -> s
      Pair a b -> go a <> go b
    dedupe = \case
      [] -> []
      (n, l, ids) : rest ->
        let others = [(l',more) | (n',l',more) <- rest, n' == n, more == ids]
         in (n, if all ((== l) . fst) others then l else Nothing, nub (ids <> concatMap snd others))
              : dedupe [x | x@(n',_,more) <- rest, n' /= n || more /= ids]

-- | The two sides of a @|||@ joining again: where both made a pair, so does
-- the join.
merge :: From -> From -> From
merge (Pair a b) (Pair c d) = Pair (merge a c) (merge b d)
merge x y = Sources (sources (Pair x y))

-- | A counter for ids, the items of each open box (innermost first), and edges.
data BuildState = BuildState
  { next :: Int, open :: [[Item]], links :: [Edge]
  , current :: Address, mapped :: [(Address, Text)]
  }

newtype Build a = Build {runBuild :: BuildState -> (a, BuildState)}

instance Functor Build where
  fmap f (Build g) = Build (\s -> let (a, s') = g s in (f a, s'))

instance Applicative Build where
  pure a = Build (\s -> (a, s))
  Build f <*> Build g = Build (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))

instance Monad Build where
  Build g >>= k = Build (\s -> let (a, s1) = g s in (k a).runBuild s1)

itemId :: Address -> Int -> Text
itemId a n = "n_" <> T.replace "/" "_" (addressText a) <> "_" <> T.pack (show n)

fresh :: Build Text
fresh = Build $ \s ->
  let i = itemId s.current s.next
   in (i, s {next = s.next + 1, mapped = (s.current,i):s.mapped})

located :: Address -> Build a -> Build a
located a (Build f) = Build $ \s ->
  let (result, out) = f s {current = a, next = 0}
   in (result, out {current = s.current, next = s.next})

item :: Item -> Build ()
item i = Build $ \s -> ((), s {open = case s.open of
  current:outer -> (i:current):outer
  [] -> [[i]]})

edge :: Edge -> Build ()
edge e = Build (\s -> ((), s {links = e:s.links}))

edgeCount :: Build Int
edgeCount = Build (\s -> (length s.links,s))

entriesSince :: Int -> [Text] -> Build [Text]
entriesSince before sources = Build $ \s ->
  let new = reverse (take (length s.links - before) s.links)
   in (foldr (\x acc -> x : filter (/= x) acc) [] [e.to | e <- new, e.from `elem` sources],s)

connect :: From -> Text -> Build ()
connect from to = mapM_ (\(f,l,ids) -> edge (Edge f to l Flow Nothing ids)) (sources from)

node :: From -> [Text] -> Build From
node from label = do
  n <- fresh
  item (ItemNode n StepNode label)
  connect from n
  pure (Sources [(n,Nothing,[])])

box :: [Text] -> Build a -> Build (Text,a)
box label inside = do
  b <- fresh
  Build (\s -> ((),s {open = []:s.open}))
  a <- inside
  Build $ \s -> ((),s {open = case s.open of
    contents:parent:outer -> (ItemBox b label (reverse contents):parent):outer
    other -> other})
  pure (b,a)

build :: Context -> From -> Description -> Build From
build context from = \case
  Located a d -> located a (build context from d)
  FirstOnly d -> case from of
    Pair a b -> (\x -> Pair x b) <$> build InBranch a d
    _ -> (\x -> Pair x from) <$> build InBranch from d
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
      Pair _ _ -> Sources [(n, Nothing, ids) | (n, _, ids) <- sources from]
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
  Branch l r -> decision Nothing from l r
  ForEach f -> Sources . sources . snd <$> box ["each"] (build InSequence (Sources (sources from)) f)
  -- "Again" goes back to where the body starts: the steps the loop's input
  -- flows into. If the body has none, it goes to the box.
  Repeated f -> do
    before <- edgeCount
    (b, exits) <- box ["repeatUntil"] (build InSequence from f)
    entries <- entriesSince before ([f | (f,_,_) <- sources from])
    let targets = if null entries then [b] else entries
    mapM_ (\(e, _, ids) -> mapM_ (\t -> edge (Edge e t (Just "again") Again Nothing ids)) targets) (sources exits)
    -- The condition is checked before the first run, so the input can leave
    -- without going through the body.
    pure (merge from exits)
  Annotated n (Located a (Branch l r)) -> do
    exits <- located a (decision (Just n.name) from l r)
    Build (\s -> ((),s {mapped = (s.current,itemId a 0):s.mapped}))
    pure exits
  Annotated n (Located a (Leaf info)) | not (passes (Leaf info)) -> do
    exits <- located a (step (Just n) info)
    Build (\s -> ((),s {mapped = [(s.current,i) | (i,_,_) <- sources exits] <> s.mapped}))
    pure exits
  Annotated n (Leaf info) | not (passes (Leaf info)) -> step (Just n) info
  Annotated n f -> snd <$> box (n.name : maybe [] pure n.description) (build InSequence from f)
  where
    labelled l = Sources [(f, Just l, ids) | (f, _, ids) <- sources from]
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
          mapM_ (drawTool exits) tools
        _ -> pure ()
      pure exits

decision :: Maybe Text -> From -> Description -> Description -> Build From
decision name from left right = do
  diamond <- fresh
  item (ItemNode diamond DecisionNode (maybe [] pure name))
  connect from diamond
  address <- Build (\s -> (s.current,s))
  let branch side label = build InBranch (Sources [(diamond,Just label,[EdgeId address side])])
  merge <$> branch LeftSide "left" left <*> branch RightSide "right" right

-- Definitions are finite. References link to the definition's address without
-- recursively drawing it again.
drawTool :: From -> ToolInfo -> Build ()
drawTool from t = located t.address $ case t.body of
  ToolFlow d -> do
    let n = "tool_" <> itemId t.address 0
    Build (\s -> ((), s {mapped = (t.address,n):s.mapped}))
    item (ItemNode n ToolNode ["tool " <> t.name])
    mapM_ (\(e,_,_) -> edge (Edge e n Nothing Uses Nothing [])) (sources from)
    exits <- build InSequence (Sources [(n,Nothing,[])]) d
    let end = "return_" <> itemId t.address 0
    item (ItemNode end Terminal ["return"])
    connect exits end
  ToolGroup _ members -> do
    (b,()) <- box ["recursive tools", "entry " <> t.name] (mapM_ (drawTool (Sources [])) members)
    mapM_ (\(e,_,_) -> edge (Edge e b Nothing Uses Nothing [])) (sources from)
  ToolReference target -> do
    n <- fresh
    item (ItemNode n ToolNode ["tool " <> t.name])
    mapM_ (\(e,_,_) -> edge (Edge e n Nothing Uses Nothing [])) (sources from)
    edge (Edge n ("tool_" <> itemId target 0) (Just "reference") Reference (Just target) [])

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

-- The logical root is available to consumers without adding a diagram box.
drawingItems :: FlowGraph -> [Item]
drawingItems graph = case graph.items of
  [ItemBox root _ contents] | root == itemId rootAddress 0 -> contents
  items -> items

-- | A Mermaid flowchart of the flow's graph.
mermaid :: Description -> Text
mermaid d = T.unlines ("flowchart TD" : concatMap (items' "  ") is <> map edge' es)
  where
    graph = flowGraph d
    is = drawingItems graph
    es = graph.edges
    items' indent = \case
      ItemNode i kind ls -> [indent <> i <> shape kind (T.intercalate "<br/>" (map escape ls))]
      ItemBox i ls inside -> [indent <> "subgraph " <> i <> "[\"" <> T.intercalate "<br/>" (map escape ls) <> "\"]"] <> concatMap (items' (indent <> "  ")) inside <> [indent <> "end"]
    shape kind l = case kind of
      Terminal -> "([\"" <> l <> "\"])"
      StepNode -> "[\"" <> l <> "\"]"
      ToolNode -> "[/\"" <> l <> "\"/]"
      DecisionNode -> "{\"" <> (if T.null l then " " else l) <> "\"}"
      JunctionNode -> "((\" \"))"
    edge' (Edge f t l style _ _) = "  " <> f <> arrow style <> maybe "" (\x -> "|" <> escape x <> "|") l <> " " <> t
    arrow = \case
      Flow -> " -->"
      Uses -> " -.-"
      Again -> " -.->"
      Reference -> " -.->"
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
    graph = flowGraph d
    is = drawingItems graph
    es = graph.edges
    items' indent = \case
      ItemNode i kind ls -> [indent <> i <> " [label=\"" <> T.intercalate "\\n" (map inner ls) <> "\"" <> shape kind ls <> "];"]
      ItemBox i ls inside -> [indent <> "subgraph cluster_" <> i <> " {", indent <> "  label=\"" <> T.intercalate "\\n" (map inner ls) <> "\";", indent <> "  style=rounded;"] <> concatMap (items' (indent <> "  ")) inside <> [indent <> "}"]
    shape kind ls = case kind of
      Terminal -> ", shape=oval"
      StepNode -> ""
      ToolNode -> ", shape=parallelogram, style=\"\""
      DecisionNode -> ", shape=diamond, style=solid" <> if null ls then ", width=0.3, height=0.3, fixedsize=true" else ""
      JunctionNode -> ", shape=point, width=0.06"
    -- An edge into a box points at the box's first node, clipped to the box.
    edge' (Edge f t l style _ selections) =
      let (target, attrs) = case firstNode t is of
            Just n
              | inBox t f is -> (n, [])
              | otherwise -> (n, ["lhead=cluster_" <> t])
            Nothing -> (t, [])
          sharedTarget = length [() | e <- es, e.from == f, e.to == t, not (null e.selections)] > 1
          ports = case selections of
            [EdgeId _ LeftSide] | sharedTarget -> ["tailport=sw", "headport=w", "minlen=2"]
            [EdgeId _ RightSide] | sharedTarget -> ["tailport=se", "headport=e", "minlen=2"]
            _ -> []
          extra = maybe [] (\x -> ["label=" <> str x]) l <> attrs <> ports <> styleOf style
       in "  " <> f <> " -> " <> target <> (if null extra then "" else " [" <> T.intercalate ", " extra <> "]") <> ";"
    styleOf = \case
      Flow -> []
      Uses -> ["style=dotted", "arrowhead=none"]
      Again -> ["style=dashed"]
      Reference -> ["style=dotted"]
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

-- | The complete graph as a JSON-shaped value, without executing a flow.
-- The object contains @items@, @edges@, @addressItems@ and @bodies@ arrays.
-- Items have @id@, @kind@ ("node" or "box") and @labels@; nodes add
-- @nodeKind@ ("terminal", "step", "tool", "decision", "junction"), while
-- boxes add nested @items@. Edge @style@ is "flow", "uses", "again" or
-- "reference". Optional labels, reference targets and bindings encode as null.
-- Addresses and selection ids use 'addressText' and 'edgeIdText', matching
-- observation encodings within the same captured flow, not across revisions.
-- The logical root and unmapped graph-only junctions are preserved as supplied;
-- no runtime scopes are inferred from items or labels.
flowGraphValue :: FlowGraph -> Value
flowGraphValue graph = Object
  [ ("items", Array (map itemValue graph.items))
  , ("edges", Array (map edgeValue graph.edges))
  , ("addressItems", Array
      [Object [("address", addressValue a), ("item", String i)]
      | (a,i) <- graph.addressItems])
  , ("bodies", Array
      [Object [("owner", addressValue b.owner)
              ,("binding", maybe Null addressValue b.binding)
              ,("bodyRoot", addressValue b.bodyRoot)]
      | b <- graph.bodies])
  ]
 where
  addressValue = String . addressText
  labelsValue = Array . map String
  itemValue = \case
    ItemNode i kind labels -> Object
      [("id", String i), ("kind", String "node")
      ,("nodeKind", String (nodeTag kind)), ("labels", labelsValue labels)]
    ItemBox i labels children -> Object
      [("id", String i), ("kind", String "box")
      ,("labels", labelsValue labels), ("items", Array (map itemValue children))]
  nodeTag = \case
    Terminal -> "terminal"
    StepNode -> "step"
    ToolNode -> "tool"
    DecisionNode -> "decision"
    JunctionNode -> "junction"
  edgeValue e = Object
    [("from", String e.from), ("to", String e.to)
    ,("label", maybe Null String e.label), ("style", String (styleTag e.style))
    ,("referenceTarget", maybe Null addressValue e.referenceTarget)
    ,("selections", Array (map (String . edgeIdText) e.selections))]
  styleTag = \case
    Flow -> "flow"
    Uses -> "uses"
    Again -> "again"
    Reference -> "reference"

-- | The description as a JSON-shaped value, for UIs and other agents.
-- Sequence/parallel lists are flattened for reading; element addresses survive.
-- Use the graph's address mapping for intermediate composition scopes.
descriptionValue :: Description -> Value
descriptionValue = \case
  Located a d -> case descriptionValue d of
    Object fields -> Object (("address",String (addressText a)):fields)
    value -> value
  FirstOnly d -> Object [("kind",String "first"),("step",descriptionValue d)]
  Leaf info -> leaf info
  Sequence ds -> node "sequence" [("steps", Array (map descriptionValue (concatMap sequenceParts ds)))]
  Together ds -> node "together" [("steps", Array (map descriptionValue (concatMap togetherParts ds)))]
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
    sequenceParts (Located _ (Sequence ds)) = concatMap sequenceParts ds
    sequenceParts (Sequence ds) = concatMap sequenceParts ds
    sequenceParts d = [d]
    togetherParts (Located _ (Together ds)) = concatMap togetherParts ds
    togetherParts (Together ds) = concatMap togetherParts ds
    togetherParts d = [d]
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
