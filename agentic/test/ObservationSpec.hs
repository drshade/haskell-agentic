module ObservationSpec (spec) where

import Agentic
import Agentic.Runtime (Conversation (..))
import Agentic.Scripted (scripted)
import Control.Exception (evaluate, throwIO, ErrorCall (..), SomeException, try)
import Data.IORef
import Data.List (nub, inits)
import Data.Text (Text)
import Data.Text qualified as T
import ObservationModel
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

capture :: Runtime IO -> Agentic IO a b -> a -> IO (b,[Summary])
capture rt flow input = do
  ref <- newIORef []
  result <- interpret (observing (\e -> modifyIORef' ref (summarise e:)) rt) flow input
  events <- reverse <$> readIORef ref
  validate flow events
  pure (result,events)

validate :: Agentic IO a b -> [Summary] -> Expectation
validate flow events = do
  let graph = flowGraph (either (error . show) id (describe flow))
  let ids = itemIds graph.items
      addresses = map fst graph.addressItems
      selections = nub (concatMap (.selections) graph.edges)
  length ids `shouldBe` length (nub ids)
  all (\e -> e.from `elem` ids && e.to `elem` ids) graph.edges `shouldBe` True
  all ((`elem` ids) . snd) graph.addressItems `shouldBe` True
  all (\e -> case e.referenceTarget of
    Nothing -> e.style /= Reference
    Just target -> e.style == Reference && target `elem` addresses) graph.edges `shouldBe` True
  length (nub (map addressText addresses)) `shouldBe` length (nub addresses)
  length (nub (map edgeIdText selections)) `shouldBe` length selections
  prefixesValid graph events `shouldBe` True
  case replay graph events of
    Left problem -> expectationFailure problem
    Right (State rows) -> [key | (key,_,_,Open) <- rows] `shouldBe` []

entries :: NodeType -> [Summary] -> [Summary]
entries kind es = [e | e <- es, e.nodeType == kind, Entered _ <- [e.happened]]

spec :: Spec
spec = H.describe "execution observations" $ do
  it "identifies unnamed and identically labelled nodes, including hidden plumbing" $ do
    let f = (arr (+1) `named` "same") >>> (arr (*2) `named` "same") :: Agentic IO Int Int
    (result,es) <- capture runtime f 4
    result `shouldBe` 10
    let started = [e.occurrence | e <- es, Entered _ <- [e.happened]]
    length started `shouldBe` length (nub started)
    length (entries PureNode es) `shouldBe` 2
    length (entries NoteNode es) `shouldBe` 2
    let graph = flowGraph (either (error . show) id (describe f))
    length (nub (map (addressText . fst) graph.addressItems)) `shouldBe` length graph.addressItems
    length (nub (map occurrenceValue started)) `shouldBe` length started

  it "keeps the JSON sequence flat while publishing intermediate addresses" $ do
    let f = act pure >>> act pure >>> act pure :: Agentic IO Int Int
        d = either (error . show) id (describe f)
        graph = flowGraph d
    case descriptionValue d of
      Object fields -> case lookup "steps" fields of
        Just (Array steps) -> do
          length steps `shouldBe` 3
          all (\case
            Object fs -> case lookup "address" fs of Just (String _) -> True; _ -> False
            _ -> False) steps `shouldBe` True
        _ -> expectationFailure "missing sequence steps"
      _ -> expectationFailure "missing description object"
    length graph.addressItems `shouldBe` 6 -- run, two sequences, three effects

  it "records both logical choice edges, including a pass-through" $ do
    let f = (returnA ||| arr (+1)) :: Agentic IO (Either Int Int) Int
    (_,leftEvents) <- capture runtime f (Left 1)
    (_,rightEvents) <- capture runtime f (Right 1)
    let selections es = [edge | e <- es, Selected edge <- [e.happened]]
    selections leftEvents `shouldNotBe` selections rightEvents
    length (nub (map edgeIdText (selections leftEvents <> selections rightEvents))) `shouldBe` 2
    length (entries PureNode leftEvents) `shouldBe` 0
    length (entries IdentityNode rightEvents) `shouldBe` 0

  it "keeps both logical ids when pass-through choice edges coalesce" $ do
    let f = (returnA ||| returnA) :: Agentic IO (Either Int Int) Int
        graph = flowGraph (either (error . show) id (describe f))
    (_,ls) <- capture runtime f (Left 1)
    (_,rs) <- capture runtime f (Right 1)
    let selected = [edge | e <- ls <> rs, Selected edge <- [e.happened]]
    length selected `shouldBe` 2
    [edge.selections | edge <- graph.edges, length edge.selections == 2] `shouldBe` [selected]

  it "publishes Each body roots and counts direct body returns for compound bodies" $ do
    let f = each ((arr (+1) `named` "increment") >>> arr (*2)) :: Agentic IO [Int] [Int]
    (result,es) <- capture runtime f [1,2,3]
    result `shouldBe` [4,6,8]
    let eachEntry = one (entries EachNode es)
        bodies = [e | e <- es, e.parent == Just eachEntry.occurrence, Entered _ <- [e.happened]]
        graph = flowGraph (either (error . show) id (describe f))
    eachEntry.happened `shouldBe` Entered (Just 3)
    map (.nodeType) bodies `shouldBe` replicate 3 SequenceNode
    map (last . (.context) . (.occurrence)) bodies `shouldBe` [EachFrame eachEntry.occurrence.address i 3 | i <- [0..2]]
    [r.bodyRoot | r <- graph.bodies, r.owner == eachEntry.occurrence.address] `shouldBe` nub (map ((.address) . (.occurrence)) bodies)
    (_,empty) <- capture runtime f []
    map (.happened) (entries EachNode empty) `shouldBe` [Entered (Just 0)]
    length (entries SequenceNode empty) `shouldBe` 0

  it "attributes a failed Each spine to the enclosing scope, without entry" $ do
    ref <- newIORef []
    let rt = observing (\e -> modifyIORef' ref (summarise e:)) runtime
        f = each (arr id) :: Agentic IO [Int] [Int]
    interpret rt f (1:error "tail") `shouldThrow` errorCall "tail"
    es <- reverse <$> readIORef ref
    map (.happened) es `shouldBe` [Entered Nothing, Failed]

  it "does not force elements or a returned value even to WHNF" $ do
    (unforced,_) <- capture runtime (each (arr (const (1 :: Int)))) [error "input element" :: Int]
    unforced `shouldBe` [1]
    (_,es) <- capture runtime (each (arr (const (error "element" :: Int)))) [1::Int]
    length (entries PureNode es) `shouldBe` 1
    (value,_) <- capture runtime (arr (const (error "later" :: Int))) ()
    evaluate value `shouldThrow` errorCall "later"

  it "attributes delayed failure to the scope that demands it" $ do
    ref <- newIORef []
    let rt = observing (\e -> modifyIORef' ref (summarise e:)) runtime
        f = arr (const (error "later" :: Int)) >>> act evaluate
    interpret rt f () `shouldThrow` errorCall "later"
    es <- reverse <$> readIORef ref
    [e.happened | e <- es, e.nodeType == PureNode] `shouldBe` [Entered Nothing, Returned Completed]
    [e.happened | e <- es, e.nodeType == EffectNode] `shouldBe` [Entered Nothing, Failed]
    validate f es

  it "records zero and multiple Repeat passes without a total" $ do
    let f = repeatUntil (>=3) (arr (+1)) :: Agentic IO Int Int
    (_,zero) <- capture runtime f 3
    length (entries PureNode zero) `shouldBe` 0
    (_,es) <- capture runtime f 0
    let loop = one (entries RepeatNode es)
    loop.happened `shouldBe` Entered Nothing
    map (last . (.context) . (.occurrence)) (entries PureNode es) `shouldBe` [RepeatFrame loop.occurrence.address i | i <- [0..2]]

  it "separates duplicate call ids within/across turns, rejected calls and body entry" $ do
    let t = tool @Int @Int "same" "" (arr (+1))
        f = draftWith @Int @Int @IO [t] "run"
        c name value = ToolCall "duplicate" name value
    model <- scripted [CallTools [c "same" (Integer 1), c "same" (Integer 2), c "missing" Null, c "same" Null], Respond Null, CallTools [c "same" (Integer 3)], Respond (Integer 5)]
    (_,es) <- capture runtime{systemTwo=model} f 0
    let calls = entries CallNode es
    length calls `shouldBe` 5
    length (nub (map (.occurrence) calls)) `shouldBe` 5
    length (entries PureNode es) `shouldBe` 3
    length [() | e <- es, CallResolved _ _ <- [e.happened]] `shouldBe` 4
    [o | e <- es, e.nodeType == CallNode, Returned o <- [e.happened]] `shouldBe` [CallOk,CallOk,UnknownTool,InvalidInput,CallOk]
    [i | e <- es, Turned i _ <- [e.happened]] `shouldBe` [0,1,2,3]
    let graph = flowGraph (either (error . show) id (describe f))
    [(a,b) | e <- es, CallResolved a b <- [e.happened]] `shouldSatisfy` all (\(a,b) -> any (\r -> r.binding == Just a && r.bodyRoot == b) graph.bodies)

  it "keeps recursive occurrence parents dynamic and graph definitions finite" $ do
    let self = recursiveTool $ \s -> tool @Text @Text "same" "" (draftWith [s] "again")
        f = draftWith @Text @Text @IO [self,self] "start"
        call = CallTools [ToolCall "same" "same" (String "x")]
    model <- scripted (replicate 5 call <> replicate 6 (Respond (String "done")))
    (_,es) <- capture runtime{systemTwo=model} f "x"
    let calls = entries CallNode es
        graph = flowGraph (either (error . show) id (describe f))
    let displays = [openItems graph state | prefix <- inits es, Right state <- [replay graph prefix]]
        counts item = [maybe 0 length (lookup item view) | view <- displays]
        shrinksWhileOpen ns = any (\(a,b) -> a >= 2 && b == a-1 && b > 0) (zip ns (drop 1 ns))
    any (shrinksWhileOpen . counts . snd) graph.addressItems `shouldBe` True
    length calls `shouldBe` 5
    map (length . (.context) . (.occurrence)) calls `shouldBe` [1..5]
    all (\e -> Just e.occurrence /= e.parent) es `shouldBe` True
    length [() | e <- graph.edges, e.style == Reference] `shouldBe` 2
    let ids = itemIds graph.items
    length ids `shouldBe` length (nub ids)
    all (\e -> e.from `elem` ids && e.to `elem` ids) graph.edges `shouldBe` True

  it "enters and returns empty tool batches and empty judges" $ do
    model <- scripted [CallTools [],Respond (Integer 1)]
    (_,es) <- capture runtime{systemTwo=model} (draft @Int "empty") ()
    length (entries CallNode es) `shouldBe` 0
    (_,js) <- capture runtime (judge (pure ())) ()
    [e.happened | e <- js, e.nodeType == JudgeNode] `shouldBe` [Entered Nothing,Judged,Returned Completed]

  it "records preflight rejection on root before any flow node" $ do
    let self = draftWith @Text @Text @IO [tool "loop" "" self] "loop"
    ref <- newIORef []
    interpret (observing (\e -> modifyIORef' ref (summarise e:)) runtime) self "x" `shouldThrow` (== ToolNestingLimit 100)
    es <- reverse <$> readIORef ref
    map (.nodeType) es `shouldBe` [RunNode,RunNode]
    map (.happened) es `shouldBe` [Entered Nothing,Failed]

  it "leaves pure short-circuit outcomes unknown unless the runtime supplies a hook" $ do
    let raise e = Trace (Left e, [])
        rt = (runtimeWith raise) {observe = \e -> Trace (Right (), [summarise e])}
        flow = act (\() -> raise NoSystemOne) :: Agentic Trace () ()
        Trace (result,es) = interpret rt flow ()
        graph = flowGraph (either (error . show) id (describe flow))
        hook = Scope $ \enter returned exited body -> enter >> case body of
          Trace (Left problem, events) -> Trace (Right (), events) >> exited Failed Nothing >> raise problem
          Trace (Right value, events) -> Trace (Right (), events) >> returned value >> pure value
        Trace (hooked,hs) = interpret rt{scope=hook} flow ()
    result `shouldBe` Left NoSystemOne
    map (.happened) es `shouldBe` [Entered Nothing,Entered Nothing]
    prefixesValid graph es `shouldBe` True
    case closeRecording <$> replay graph es of
      Right (State rows) -> [status | (_,_,_,status) <- rows] `shouldBe` [Unknown,Unknown]
      Left problem -> expectationFailure problem
    hooked `shouldBe` result
    map (.happened) hs `shouldBe` [Entered Nothing,Entered Nothing,Failed,Failed]
    prefixesValid graph hs `shouldBe` True

  it "keeps conversation labels and capped errors independent of occurrence context" $ do
    seen <- newIORef []
    events <- newIORef []
    let provider = SystemTwo $ \c -> modifyIORef' seen (c.path:) >> pure (Turn (Raw Null) (CallTools []))
        rt = observing (\e -> modifyIORef' events (summarise e:)) (capped 1 runtime{systemTwo=provider})
        f = each (draft @Int "run" `named` "inner") `named` "outer"
    interpret rt f [()] `shouldThrow` (== TurnLimit 1)
    readIORef seen `shouldReturn` [[Note "outer" Nothing,Note "inner" Nothing]]
    es <- reverse <$> readIORef events
    [e.happened | e <- es, e.nodeType == DraftNode] `shouldBe` [Entered Nothing,Drafting,Turned 0 ToolBatch,Failed]

  it "discards optional payloads without evaluating them" $ do
    (_,es) <- capture runtime (arr id :: Agentic IO Int Int) 1
    let e = case es of first:_ -> first; [] -> error "no observations"
        event = Event e.occurrence e.parent e.nodeType e.path e.happened (error "discarded")
    evaluate (summarise event) `shouldReturn` e

  it "fully encodes metadata without forcing results or call inputs" $ do
    (_,es) <- capture runtime (arr (const (error "result" :: Int))) ()
    evaluate (sum [T.length (renderJson (summaryValue e)) | e <- es]) >>= (`shouldSatisfy` (>0))
    model <- scripted [CallTools [ToolCall "id" "missing" (error "input")],Respond (Integer 1)]
    (_,cs) <- capture runtime{systemTwo=model} (draft @Int "run") ()
    evaluate (sum [T.length (renderJson (summaryValue e)) | e <- cs]) >>= (`shouldSatisfy` (>0))

  it "treats an interpret inside an act as a separately namespaced run" $ do
    outer <- newIORef []
    inner <- newIORef []
    let inside = observing (\e -> modifyIORef' inner (summarise e:)) runtime
        outside = observing (\e -> modifyIORef' outer (summarise e:)) runtime
    interpret outside (act (interpret inside (arr id :: Agentic IO Int Int))) 1 `shouldReturn` 1
    mapM_ (\ref -> do
      es <- reverse <$> readIORef ref
      let roots = entries RunNode es
      map (.parent) roots `shouldBe` [Nothing]
      map ((.context) . (.occurrence)) roots `shouldBe` [[]]) [outer,inner]

  it "does not retry a synchronous error in a child's entry observer" $ do
    ref <- newIORef []
    let rt = observing (\e -> do
          modifyIORef' ref (summarise e:)
          if e.nodeType == PureNode then throwIO (userError "entry") else pure ()) runtime
    interpret rt (arr id :: Agentic IO Int Int) 1 `shouldThrow` anyIOException
    es <- reverse <$> readIORef ref
    [e.happened | e <- es, e.nodeType == PureNode] `shouldBe` [Entered Nothing]
    (last es).happened `shouldBe` Failed

  it "preserves body exceptions when failure reporting throws" $ do
    let rt = observing (\e -> if e.happened == Failed then throwIO (userError "observer") else pure ()) runtime
    interpret rt (act (\() -> throwIO (ErrorCall "body")) :: Agentic IO () ()) () `shouldThrow` errorCall "body"

  it "does not send a second terminal when a return observer throws" $ do
    ref <- newIORef []
    let rt = observing (\e -> do
          modifyIORef' ref (summarise e:)
          if e.nodeType == PureNode && e.happened == Returned Completed then throwIO (userError "observer") else pure ()) runtime
    _ <- try (interpret rt (arr id :: Agentic IO Int Int) 1) :: IO (Either SomeException Int)
    es <- reverse <$> readIORef ref
    [e.happened | e <- es, e.nodeType == PureNode] `shouldBe` [Entered Nothing,Returned Completed]
    (last es).happened `shouldBe` Failed

itemIds :: [Item] -> [Text]
itemIds = concatMap (\case ItemNode i _ _ -> [i]; ItemBox i _ xs -> i:itemIds xs)

one :: [a] -> a
one [a] = a
one _ = error "expected one occurrence"

-- A writer outside Either keeps the prefix when the action short-circuits.
newtype Trace a = Trace (Either FlowError a, [Summary])
instance Functor Trace where
  fmap f (Trace (a,es)) = Trace (fmap f a,es)
instance Applicative Trace where
  pure a = Trace (Right a,[])
  f <*> a = f >>= (\g -> fmap g a)
instance Monad Trace where
  Trace (Left e,es) >>= _ = Trace (Left e,es)
  Trace (Right a,es) >>= f = case f a of Trace (b,more) -> Trace (b,es <> more)
