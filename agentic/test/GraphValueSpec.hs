module GraphValueSpec (spec) where

import Agentic hiding (field)
import Agentic.Scripted (scripted)
import Data.IORef
import Data.List (nub)
import Data.Text (Text)
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

field :: Text -> Value -> Value
field k v = maybe (error ("missing field: " <> show k)) id (optionalField k v)

optionalField :: Text -> Value -> Maybe Value
optionalField k (Object fs) = lookup k fs
optionalField _ _ = Nothing

array :: Value -> [Value]
array (Array xs) = xs
array v = error ("expected array: " <> show v)

only :: [Value] -> Value
only [v] = v
only vs = error ("expected one item: " <> show vs)

flatten :: Value -> [Value]
flatten v = v : case field "kind" v of
  String "box" -> concatMap flatten (array (field "items" v))
  _ -> []

graphOf :: Agentic m a b -> FlowGraph
graphOf = flowGraph . either (error . show) id . describe

spec :: Spec
spec = H.describe "flowGraphValue" $ do
  it "preserves hidden aliases, the logical root and unmapped routing junctions" $ do
    let f = ((returnA &&& act pure) ||| (returnA &&& act pure))
            >>> arr fst >>> arr (+1) :: Agentic IO (Either Int Int) Int
        g = graphOf f
        v = flowGraphValue g
        nodes = concatMap flatten (array (field "items" v))
        mappings = array (field "addressItems" v)
        junctions = [field "id" n | n <- nodes, optionalField "nodeKind" n == Just (String "junction")]
        ids = map (field "id") nodes
    field "labels" (only (array (field "items" v))) `shouldBe` Array [String "run"]
    null junctions `shouldBe` False
    all (`notElem` map (field "item") mappings) junctions `shouldBe` True
    map (field "address") mappings `shouldBe` map (String . addressText . fst) g.addressItems
    map (field "item") mappings `shouldBe` map (String . snd) g.addressItems
    length (nub (map snd g.addressItems)) `shouldSatisfy` (< length g.addressItems)
    all (`elem` ids) (map (field "item") mappings) `shouldBe` True
    mapM_ (\e -> do
      field "from" e `shouldSatisfy` (`elem` ids)
      field "to" e `shouldSatisfy` (`elem` ids)) (array (field "edges" v))

  it "matches both selected edges and occurrence addresses to summary encodings" $ do
    let f = (returnA ||| act pure) :: Agentic IO (Either Int Int) Int
        v = flowGraphValue (graphOf f)
        addresses = map (field "address") (array (field "addressItems" v))
        selections = concatMap (array . field "selections") (array (field "edges" v))
    ref <- newIORef []
    let rt = observing (\e -> modifyIORef' ref (summarise e:)) runtime
    interpret rt f (Left 1) `shouldReturn` 1
    interpret rt f (Right 2) `shouldReturn` 2
    es <- readIORef ref
    let roots = [e.occurrence.address | e <- es, e.nodeType == RunNode]
    null roots `shouldBe` False
    mapM_ (\a -> field "root" v `shouldBe` maybe Null String (lookup a (graphOf f).addressItems)) roots
    field "root" v `shouldBe` field "id" (only (array (field "items" v)))
    length selections `shouldBe` 2
    [String (edgeIdText edge) | e <- es, Selected edge <- [e.happened]] `shouldMatchList` selections
    mapM_ (\e -> field "address" (occurrenceValue e.occurrence) `shouldSatisfy` (`elem` addresses)) es
    -- The same edge string is also exposed by summaryValue.
    mapM_ (\e -> case e.happened of
      Selected edge -> field "edge" (field "event" (summaryValue e)) `shouldBe` String (edgeIdText edge)
      _ -> pure ()) es

  it "keeps loop ownership and reused recursive tool call targets finite" $ do
    let self = recursiveTool $ \s -> tool @Text @Text "same" "" (draftWith [s] "again")
        f = each (draftWith @Text @Text @IO [self,self] "start")
        g = graphOf f
        v = flowGraphValue g
        relations = array (field "bodies" v)
        addresses = map (field "address") (array (field "addressItems" v))
        refs = [e | e <- array (field "edges" v), field "style" e == String "reference"]
    length refs `shouldBe` 2
    map (field "referenceTarget") refs `shouldSatisfy` all (`elem` addresses)
    map (field "owner") relations `shouldBe` map (String . addressText . (.owner)) g.bodies
    map (field "binding") relations `shouldBe` map (maybe Null (String . addressText) . (.binding)) g.bodies
    map (field "bodyRoot") relations `shouldBe` map (String . addressText . (.bodyRoot)) g.bodies
    Null `shouldSatisfy` (`elem` map (field "binding") relations)
    model <- scripted [CallTools [ToolCall "call" "same" (String "x")], Respond (String "inner"), Respond (String "outer")]
    seen <- newIORef []
    interpret (observing (\e -> modifyIORef' seen (summarise e:)) runtime{systemTwo=model}) f ["x"] `shouldReturn` ["outer"]
    es <- readIORef seen
    let resolved = [(String (addressText a),String (addressText b)) | e <- es, CallResolved a b <- [e.happened]]
    length resolved `shouldBe` 1
    resolved `shouldSatisfy` all (\(a,b) -> any (\r -> field "binding" r == a && field "bodyRoot" r == b) relations)

  it "uses explicit node and edge tags and retains labels and nullable fields" $ do
    let kinds = [Terminal,StepNode,ToolNode,DecisionNode,JunctionNode]
        styles = [Flow,Uses,Again,Reference]
        g = FlowGraph [ItemBox "root" ["box"] [ItemNode "node" k ["line one","line two"] | k <- kinds]]
              [Edge "root" "node" label style Nothing [] | style <- styles, label <- [Nothing,Just "label"]] [] []
        v = flowGraphValue g
        ns = array (field "items" (only (array (field "items" v))))
        es = array (field "edges" v)
    map (field "nodeKind") ns `shouldBe` map String ["terminal","step","tool","decision","junction"]
    map (field "labels") ns `shouldBe` replicate 5 (Array [String "line one",String "line two"])
    map (field "style") es `shouldBe` concatMap (replicate 2 . String) ["flow","uses","again","reference"]
    map (field "label") es `shouldBe` concat (replicate 4 [Null,String "label"])
    map (field "referenceTarget") es `shouldBe` replicate 8 Null

  it "encodes a repeat body without executing effects or providers" $ do
    let f = repeatUntil (const True) (act (\_ -> error "must not execute") >>> draft @Int @Int "unused") :: Agentic IO Int Int
        v = flowGraphValue (graphOf f)
    length (array (field "bodies" v)) `shouldBe` 1
    map (field "style") (array (field "edges" v)) `shouldSatisfy` elem (String "again")
    length (show v) `shouldSatisfy` (> 0)
