{-# LANGUAGE BangPatterns #-}
module RecursionSpec (spec) where

import Agentic
import Agentic.Scripted
import Control.Exception (evaluate)
import Data.IORef
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

data Hidden = Hidden
instance Contract Hidden where
  contract = scalar (ScalarInfo "test.hidden" []) Nothing

type TextTool = Tool IO

textTool :: Text -> Agentic IO Text Text -> TextTool
textTool name = tool name "recursive test"

root :: [TextTool] -> Agentic IO Text Text
root ts = draftWith ts "run"

call :: Text -> Action
call name = CallTools [ToolCall "reused-id" name (String "input")]

done :: Action
done = Respond (String "done")

scriptFor :: [Text] -> [Action]
scriptFor names = map call names <> replicate (length names + 1) done

run :: Agentic IO Text Text -> [Action] -> IO [Happened]
run flow actions = do
  seen <- newIORef []
  model <- scripted actions
  let rt = observing (\e -> modifyIORef' seen (e.happened :)) runtime{systemTwo = model}
  interpret rt flow "input" `shouldReturn` "done"
  reverse <$> readIORef seen

finite :: Agentic m i o -> Description
finite = either (error . show) id . describe

-- Inspect actual finite tool definitions/references, without relying on labels.
toolsIn :: Description -> [ToolInfo]
toolsIn = \case
  Leaf (DraftInfo _ _ _ ts) -> concatMap collect ts
  Leaf _ -> []
  Sequence ds -> concatMap toolsIn ds
  Together ds -> concatMap toolsIn ds
  Halves l r -> toolsIn l <> toolsIn r
  Branch l r -> toolsIn l <> toolsIn r
  ForEach d -> toolsIn d
  Repeated d -> toolsIn d
  Annotated _ d -> toolsIn d
 where
  collect t = t : case t.body of
    ToolFlow d -> toolsIn d
    ToolGroup _ members -> concatMap collect members
    ToolReference _ -> []

checkFinite :: Int -> Agentic IO Text Text -> Expectation
checkFinite count f = do
  let ts = toolsIn (finite f)
      defs = [t.address | t <- ts, ToolFlow _ <- [t.body]]
      refs = [target | t <- ts, ToolReference target <- [t.body]]
  length defs `shouldBe` count
  length (nub defs) `shouldBe` count
  all (`elem` defs) refs `shouldBe` True
  renderJson (descriptionValue (finite f)) `shouldSatisfy` T.isInfixOf "\"body\""

spec :: Spec
spec = H.describe "recursive tools" $ do
  it "runs self recursion past 100 with one finite body" $ do
    let self = recursiveTool $ \s -> textTool "self" (root [s])
        f = root [self]
    checkFinite 1 f
    events <- run f (scriptFor (replicate 126 "self"))
    length [() | ToolCalled _ <- events] `shouldBe` 126
    length [() | ToolReturned _ _ <- events] `shouldBe` 126

  it "runs mutual recursion and exposes each group definition once" $ do
    let (ping, _) = recursiveTools $ \(p,q) ->
          (textTool "ping" (root [q]), textTool "pong" (root [p]))
        f = root [ping]
        names = take 126 (cycle ["ping", "pong"])
    checkFinite 2 f
    events <- run f (scriptFor names)
    [c.name | ToolCalled c <- events] `shouldBe` names

  it "restores outer lexical meaning during nested A/B/A/B reentry" $ do
    entered <- newIORef ([] :: [Text])
    let mark name = act (\x -> modifyIORef' entered (<> [name]) >> pure x)
        a = recursiveTool $ \a' ->
          let b = recursiveTool $ \_ -> textTool "B" (mark "B" >>> root [a'])
           in textTool "A" (mark "A" >>> root [b])
        names = take 126 (cycle ["A", "B"])
    checkFinite 2 (root [a])
    _ <- run (root [a]) (scriptFor names)
    readIORef entered `shouldReturn` names

  it "keeps sibling groups and reuse at separate flow positions distinct" $ do
    entered <- newIORef ([] :: [Text])
    let leaf tag = recursiveTool $ \_ -> textTool "same" $
          act (\x -> modifyIORef' entered (<> [tag]) >> pure x)
        a = leaf "a"
        b = leaf "b"
        f = root [a] >>> root [b] >>> root [a]
    checkFinite 3 f
    _ <- run f (concat (replicate 3 [call "same", done]))
    readIORef entered `shouldReturn` ["a", "b", "a"]

  it "preserves a tool saved by an act when used in an unrelated binder" $ do
    saved <- newIORef Nothing
    oldRuns <- newIORef (0 :: Int)
    newRuns <- newIORef (0 :: Int)
    let owner = recursiveTool $ \ref -> textTool "same" $
          act (\x -> modifyIORef' oldRuns (+1) >> writeIORef saved (Just ref) >> pure x)
    _ <- run (root [owner]) [call "same", done]
    escaped <- readIORef saved >>= maybe (fail "tool was not saved") pure
    let unrelated = recursiveTool $ \_ -> textTool "same" $
          act (\x -> modifyIORef' newRuns (+1) >> pure x) >>> root [escaped]
    checkFinite 2 (root [unrelated])
    _ <- run (root [unrelated]) [call "same", call "same", done, done]
    readIORef oldRuns `shouldReturn` 2
    readIORef newRuns `shouldReturn` 1
    -- The escaped member is also valid without any enclosing binder.
    _ <- run (root [escaped]) [call "same", done]
    readIORef oldRuns `shouldReturn` 3

  it "supports strict tuple patterns, triples and metadata-selected bodies" $ do
    let (a,b,c) = recursiveTools $ \ !(a',b',c') ->
          (tool "a" "a-doc" (if toolName b' == "b" && toolDescription c' == "c-doc" then root [b'] else root [a']),
           tool "b" "b-doc" (root [c']), tool "c" "c-doc" (root [a']))
    map toolName [a,b,c] `shouldBe` ["a", "b", "c"]
    checkFinite 3 (root [a])
    _ <- run (root [a]) (scriptFor (take 126 (cycle ["a", "b", "c"])))
    pure ()

  it "allows acyclic dependencies between member names" $ do
    let (a,b) = recursiveTools $ \(_,b') ->
          (textTool ("a-" <> toolName b') (arr id), textTool "b" (arr id))
    map toolName [a,b] `shouldBe` ["a-b", "b"]

  it "supports four- and five-member groups" $ do
    let (a,_,_,_) = recursiveTools $ \(p,q,r,s) ->
          (textTool "a" (root [q]), textTool "b" (root [r]),
           textTool "c" (root [s]), textTool "d" (root [p]))
        (v,_,_,_,_) = recursiveTools $ \(v',w,x,y,z) ->
          (textTool "v" (root [w]), textTool "w" (root [x]),
           textTool "x" (root [y]), textTool "y" (root [z]), textTool "z" (root [v']))
    checkFinite 4 (root [a])
    checkFinite 5 (root [v])
    _ <- run (root [a]) (scriptFor ["a","b","c","d","a"])
    _ <- run (root [v]) (scriptFor ["v","w","x","y","z","v"])
    pure ()

  it "rejects member aliases in preflight, describe, and metadata access" $ do
    let (a,_) = recursiveTools (\(x,y) -> (y,x)) :: (TextTool,TextTool)
        problem = InvalidRecursion "recursiveTools must return tool definitions, not member aliases"
    preflight (root [a]) `shouldBe` Left problem
    (() <$ describe (root [a])) `shouldBe` Left problem
    evaluate (toolName a) `shouldThrow` errorCall "recursiveTools must return tool definitions, not member aliases"

  it "validates uncalled members before effects and retains projection paths" $ do
    ran <- newIORef False
    let (good,_) = recursiveTools $ \_ ->
          (textTool "good" (arr id), tool "bad" "" (arr (const Hidden) :: Agentic IO Text Hidden))
        f = act (\x -> writeIORef ran True >> pure x) >>> root [good]
    interpret runtime f "input" `shouldThrow` (\case
      UnsupportedContract [Note "bad" Nothing] (ProjectionError ["tool.output"] _) -> True
      _ -> False)
    readIORef ran `shouldReturn` False
    (() <$ describe f) `shouldBe` Right ()

  it "validates unsupported contracts inside recursive bodies" $ do
    let bad = recursiveTool $ \_ -> textTool "bad" (draft @Hidden "hidden" >>> arr (const "done"))
    preflight (root [bad]) `shouldSatisfy` (\case
      Left (UnsupportedContract [Note "bad" Nothing] (ProjectionError ["draft.output"] _)) -> True
      _ -> False)

  it "allows 100 nested tool bodies and rejects 101 in both static walks" $ do
    let chain 0 = arr id
        chain n = root [textTool "next" (chain (n-1))]
    preflight (chain (100 :: Int)) `shouldBe` Right ()
    (() <$ describe (chain (100 :: Int))) `shouldBe` Right ()
    preflight (chain (101 :: Int)) `shouldBe` Left (ToolNestingLimit 100)
    (() <$ describe (chain (101 :: Int))) `shouldBe` Left (ToolNestingLimit 100)

  it "rejects the old plain knot before effects, without name-based truncation" $ do
    ran <- newIORef False
    let researcher = root [textTool "research" researcher]
        f = act (\x -> writeIORef ran True >> pure x) >>> researcher
    preflight f `shouldBe` Left (ToolNestingLimit 100)
    (() <$ describe f) `shouldBe` Left (ToolNestingLimit 100)
    interpret runtime f "input" `shouldThrow` (== ToolNestingLimit 100)
    readIORef ran `shouldReturn` False

  it "does not count ordinary combinator depth as tool nesting" $ do
    let f = foldr (>>>) (arr id) (replicate 200 (arr id)) :: Agentic IO Text Text
    preflight f `shouldBe` Right ()
    (() <$ describe f) `shouldBe` Right ()

  it "describes distinct same-name ordinary tools without dropping a body" $ do
    let f = root [textTool "same" (act pure)] >>> root [textTool "same" (root [])]
        tree = renderTree (finite f)
    checkFinite 2 f
    tree `shouldSatisfy` T.isInfixOf "tool same  act"
    tree `shouldSatisfy` T.isInfixOf "tool same  draft"
