-- | Looking at a flow without running it.
module Agentic.Describe
  ( describe
  , Description (..)
  , StepInfo (..)
  , ToolInfo (..)
  , renderTree
  , mermaid
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
  | OnFirst Description
  | Branch Description Description
  | ForEach Description
  | Annotated Note Description

data StepInfo
  = Glue
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
  First f -> OnFirst (describe f)
  Choose f g -> Branch (describe f) (describe g)
  Each f -> ForEach (describe f)
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

-- | The tree view. Unnamed glue is hidden, and each tool's body is expanded
-- the first time the tool appears.
renderTree :: Description -> Text
renderTree = T.intercalate "\n" . concatMap (draw "" "") . snd . trees []

-- | Convert to trees, threading the names of tools already expanded.
trees :: [Text] -> Description -> ([Text], [Tree])
trees seen = \case
  Leaf Glue -> (seen, [])
  Leaf Effect -> (seen, [Node "act" []])
  Leaf (JudgeInfo _ qs) -> (seen, [Node (judgeText qs) []])
  Leaf (DraftInfo instruction _ out tools) ->
    let (seen', toolTrees) = mapAccumL toolTree seen tools
     in (seen', [Node ("draft " <> typeLabel out <> "  " <> quoted (instructionText instruction)) toolTrees])
  Sequence ds -> concat <$> mapAccumL trees seen ds
  Together ds -> case concat <$> mapAccumL trees seen ds of
    (seen', [t]) -> (seen', [t])
    (seen', ts) -> (seen', [Node "together" ts])
  OnFirst d -> fmap (\ts -> [Node "on first" ts]) (trees seen d)
  Branch l r ->
    let (seen1, ls) = trees seen l
        (seen2, rs) = trees seen1 r
     in (seen2, [Node "branch" [labelled "left" ls, labelled "right" rs]])
  ForEach d -> case trees seen d of
    (seen', [Node "together" ts]) -> (seen', [Node "each" ts])
    (seen', ts) -> (seen', [Node "each" ts])
  Annotated n d -> case trees seen d of
    (seen', [Node t cs]) -> (seen', [Node (noteName n <> "  " <> t) cs])
    (seen', []) -> (seen', [Node (noteName n) []])
    (seen', ts) -> (seen', [Node (noteName n) ts])
  where
    toolTree s t
      | infoName t `elem` s = (s, Node ("tool " <> infoName t <> "  (see above)") [])
      | otherwise = case trees (infoName t : s) (infoBody t) of
          (s', [Node body cs]) -> (s', Node ("tool " <> infoName t <> "  " <> body) cs)
          (s', ts) -> (s', Node ("tool " <> infoName t) ts)
    labelled l = \case
      [Node t cs] -> Node (l <> " → " <> t) cs
      [] -> Node (l <> " → pass") []
      ts -> Node l ts

judgeText :: [QuestionSpec] -> Text
judgeText = \case
  [q] -> "judge " <> questionText q
  qs -> "judge " <> T.intercalate "; " (map questionText qs)
  where
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

-- | A Mermaid flowchart of the tree view.
mermaid :: Description -> Text
mermaid d = T.unlines ("flowchart TD" : concatMap nodeLines numbered <> edges)
  where
    numbered = snd (mapAccumL number (0 :: Int) (snd (trees [] d)))
    number n (Node t cs) =
      let (n', cs') = mapAccumL number (n + 1) cs
       in (n', Numbered n t cs')
    nodeLines (Numbered n t cs) = ("  n" <> tshow n <> "[\"" <> escape t <> "\"]") : concatMap nodeLines cs
    edges = zipWith edge numbered (drop 1 numbered) <> concatMap childEdges numbered
    childEdges parent@(Numbered _ _ cs) = map (edge parent) cs <> concatMap childEdges cs
    edge (Numbered a _ _) (Numbered b _ _) = "  n" <> tshow a <> " --> n" <> tshow b
    escape = T.replace "\"" "#quot;"
    tshow = T.pack . show

data Numbered = Numbered Int Text [Numbered]

-- ---------------------------------------------------------------------------
-- JSON

-- | The description as a JSON-shaped value, for UIs and other agents.
toValue :: Description -> Value
toValue = \case
  Leaf info -> leaf info
  Sequence ds -> node "sequence" [("steps", Array (map toValue ds))]
  Together ds -> node "together" [("steps", Array (map toValue ds))]
  OnFirst d -> node "onFirst" [("step", toValue d)]
  Branch l r -> node "branch" [("left", toValue l), ("right", toValue r)]
  ForEach d -> node "each" [("step", toValue d)]
  Annotated n d ->
    node "note" $
      [("name", String (noteName n))]
        <> maybe [] (\t -> [("description", String t)]) (noteDescription n)
        <> [("step", toValue d)]
  where
    node kind fields = Object (("kind", String kind) : fields)
    leaf = \case
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
