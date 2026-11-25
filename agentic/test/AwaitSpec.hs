module AwaitSpec (spec) where
import Agentic hiding (key, Tool)
import Agentic.Codec qualified as C
import Agentic.Schema (Shape (..))
import Agentic.Schema qualified as S
import Agentic.Runtime (Conversation (..))
import Agentic.Runtime qualified as R
import Agentic.Journal qualified as J
import Control.Monad (unless, forM_)
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

type Store = Map.Map Value Value
type Waits = NE.NonEmpty PendingWait

intCodec :: C.Codec Int
intCodec = C.primitive SInteger (Integer . toInteger) $ \case
  Integer n -> Right (fromInteger n)
  _ -> Left "expected integer"
awaitInt :: Agentic m Int Int
awaitInt = Step (Await intCodec intCodec)
ident :: JournalIdentity
ident = JournalIdentity "spike" "r1"

-- A pure request Program. Its algebra has store and observation operations,
-- but nothing for waiting, throwing or catching. FetchError is external.
data Tool a = Done a | Get (Store -> Tool a) | Put Store (Tool a) | Emit Happened (Tool a)
instance Functor Tool where fmap f x = x >>= pure . f
instance Applicative Tool where pure = Done; f <*> x = f >>= (\g -> g <$> x)
instance Monad Tool where
  Done a >>= f = f a
  Get k >>= f = Get (\s -> k s >>= f)
  Put s k >>= f = Put s (k >>= f)
  Emit e k >>= f = Emit e (k >>= f)
runTool :: Tool a -> Store -> (a, Store, [Happened])
runTool p s = case p of
  Done a -> (a,s,[])
  Get k -> runTool (k s) s
  Put s' k -> runTool k s'
  Emit e k -> let (a,s',es) = runTool k s in (a,s',e:es)

data FetchError = FetchError | CoreError FlowError deriving (Eq, Show)
type Guest = ExceptT FetchError Tool
baseTool :: Runtime Tool
baseTool = runtimeWith (error . show)
guest :: Runtime Guest
guest = (runtimeWith (throwE . CoreError))
  { scope = exceptTScope baseTool.scope
  , observe = \e -> ExceptT (Emit e.happened (Done (Right ())))
  , journal = Just (Journal ident (pure ())
      (\k -> ExceptT (Get (\s -> Done (Right (Map.lookup k s)))))
      (\k v -> ExceptT (Get (\s -> case Map.lookup k s of
        Just old | old /= v -> Done (Left (CoreError (JournalFailure (JournalConflict k))))
        _ -> Put (Map.insert k v s) (Done (Right ())))))) }
runGuest :: Guest a -> Store -> (Either FetchError a, Store, [Happened])
runGuest = runTool . runExceptT

fill :: Agentic Guest i o -> PendingWait -> Value -> Store -> Store
fill flow w v store = case runGuest (fulfil guest flow w v) store of
  (Right (Right ()),s,_) -> s
  result -> error (show result)

onePending :: (Either FetchError (Progress a),Store,[Happened]) -> PendingWait
onePending (Right (RunWaiting (w NE.:| [])),_,_) = w
onePending _ = error "expected exactly one pending wait"

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

behaviour :: IO ()
behaviour = do
  let run :: Monad m => Runtime m -> Agentic m i o -> i -> m (Either Waits o)
      run rt f x = convert <$> resumable rt f x
      convert (RunFinished a) = Right a
      convert (RunWaiting ws) = Left ws
      (first,s,events) = runGuest (run guest (each awaitInt) [10,20,30]) Map.empty
  waits <- case first of Right (Left ws) -> pure ws; _ -> error (show first)
  check "three reached waits" (NE.length waits == 3 && length [() | Suspended <- events] == 5)
  check "nothing saved on pending" (Map.null s)
  let (w1,w2,w3) = case NE.toList waits of [a,b,c] -> (a,b,c); _ -> error "expected three waits"
      s1 = fill (each awaitInt) w1 (Number 1.0) s
      (second,_,_) = runGuest (run guest (each awaitInt) [10,20,30]) s1
  check "one reply leaves two waits" (case second of Right (Left ws) -> NE.length ws == 2; _ -> False)
  let s2 = fill (each awaitInt) w3 (Integer 3) (fill (each awaitInt) w2 (Integer 2) s1)
      (third,_,_) = runGuest (run guest (each awaitInt) [10,20,30]) s2
  check "reply reached next step" (third == Right (Right [1,2,3]))
  check "bad reply rejected" (case runGuest (fulfil guest (each awaitInt) w1 (String "bad")) s of (Right (Left (ReplyInvalid _)),unchanged,_) -> unchanged == s; _ -> False)
  let (failure,_,failureEvents) = runGuest (run guest
        (awaitInt &&& act (const (throwE FetchError :: Guest Int))) 10) Map.empty
  check "failure stays separate" (failure == Left FetchError && Failed `elem` failureEvents)
  let observerRt = guest{observe = \e -> if e.happened == Suspended then throwE FetchError else guest.observe e}
      (observerFailure,_,obsEvents) = runGuest (run observerRt awaitInt 1) Map.empty
  check "observer failure wins" (observerFailure == Left FetchError && Failed `elem` obsEvents)
  let (seqResult,_,_) = runGuest (run guest (awaitInt >>> awaitInt) 10) Map.empty
  check "sequence frontier" (case seqResult of Right (Left ws) -> NE.length ws == 1; _ -> False)
  let flow = awaitInt &&& durablyWith intCodec intCodec (\_ -> pure . (+1))
      (mixed,saved,_) = runGuest (run guest flow 5) Map.empty
  check "sibling saved" (Map.size saved == 1 && case mixed of Right (Left _) -> True; _ -> False)
  let self = recursiveTool (\t -> tool @Int @Int "self" ""
        (draftWith [t] "recurse" >>> awaitInt))
      recursiveRt = guest{systemTwo=SystemTwo (\c -> pure $
        case (c.input,c.history) of
          (Integer n,[]) | n > 0 -> Turn (Raw (String "call"))
            (CallTools [ToolCall "same-id" "self" (Integer (n-1))])
          _ -> Turn (Raw (String "answer")) (Respond c.input))}
      recursiveFlow = draftWith [self] "recurse" :: Agentic Guest Int Int
      resumeRec st = runGuest (run recursiveRt recursiveFlow 2) st
      (rec1,rs1,_) = resumeRec Map.empty
  rw1 <- case rec1 of Right (Left (w NE.:| [])) -> pure w; _ -> error (show rec1)
  let (rec2,rs2,_) = resumeRec (fill recursiveFlow rw1 (Integer 0) rs1)
  rw2 <- case rec2 of Right (Left (w NE.:| [])) -> pure w; _ -> error (show rec2)
  check "recursive invocations have distinct wait keys" (rw1 /= rw2)
  check "recursive transport" (pendingWaitFromValue (pendingWaitValue rw1) == Right rw1)
  let (rec3,_,_) = resumeRec (fill recursiveFlow rw2 (Integer 1) rs2)
  check "recursive tool resumes" (rec3 == Right (Right 2))
  let toolRt = guest{systemTwo=SystemTwo (\c -> pure $ if null c.history
        then Turn (Raw (String "batch")) (CallTools
          [ToolCall "same" "ask" (Integer 1),ToolCall "same" "ask" (Integer 2)])
        else Turn (Raw (String "done")) (Respond (Integer 3)))}
      toolFlow = draftWith [tool @Int @Int "ask" "" awaitInt] "ask"
        :: Agentic Guest Int Int
      (toolWaits,_,_) = runGuest (run toolRt toolFlow 0) Map.empty
  check "tool batch exposes both waits" (case toolWaits of Right (Left ws) -> NE.length ws == 2; _ -> False)

spec :: Spec
spec = H.describe "external replies in a pure Program under ExceptT" $ do
  it "collects, fulfils and resumes independent and recursive waits without a suspension capability" behaviour
  it "keeps plain interpret waits on the ordinary error path" $ do
    let (result,_,_) = runGuest (interpret guest awaitInt 1) Map.empty
    case result of
      Left (CoreError (AwaitNeedsResume ws)) -> NE.length ws `shouldBe` 1
      _ -> expectationFailure (show result)
    let outer = act (\x -> interpret guest awaitInt x)
        (nested,_,events) = runGuest (resumable guest outer 1) Map.empty
    case nested of
      Left (CoreError (AwaitNeedsResume _)) -> Failed `shouldSatisfy` (`elem` events)
      _ -> expectationFailure (show nested)
  it "fails without a journal and never demands persistence codecs" $ do
    let flow = awaitWith (error "request codec forced") (error "reply codec forced") :: Agentic Guest Int Int
        (result,_,_) = runGuest (resumable guest{journal=Nothing} flow 1) Map.empty
    result `shouldBe` Left (CoreError AwaitWithoutJournal)
  it "refuses invalid identities, non-wait keys, non-await addresses and conflicting replies" $ do
    let w = onePending (runGuest (resumable guest awaitInt 1) Map.empty)
        checkFailure pending = case runGuest (fulfil guest awaitInt pending (Integer 2)) Map.empty of
          (Right (Left _),s,_) -> s `shouldBe` Map.empty
          result -> expectationFailure (show result)
    checkFailure w{R.identity=JournalIdentity "other" "r1"}
    checkFailure w{R.identity=JournalIdentity "spike" "other-revision"}
    checkFailure w{R.key=DurableKey "other" w.key.key}
    checkFailure w{R.key=w.key{J.key=w.key.key{operation=DurableEffect}}}
    let (notAwait,_,_) = runGuest (fulfil guest (act pure :: Agentic Guest Int Int) w (Integer 2)) Map.empty
    notAwait `shouldSatisfy` \case Right (Left (WaitInvalidKey _)) -> True; _ -> False
    let saved = fill awaitInt w (Integer 2) Map.empty
        (same,_,_) = runGuest (fulfil guest awaitInt w (Number 2.0)) saved
        (conflict,unchanged,_) = runGuest (fulfil guest awaitInt w (Integer 3)) saved
    same `shouldBe` Right (Right ())
    conflict `shouldSatisfy` \case Right (Left ReplyAlreadyAnswered) -> True; _ -> False
    unchanged `shouldBe` saved
  it "guards changed requests and permits a new attempt to withdraw an unvisited wait" $ do
    let w = onePending (runGuest (resumable guest awaitInt 1) Map.empty)
        saved = fill awaitInt w (Integer 2) Map.empty
        (changed,_,_) = runGuest (resumable guest awaitInt 3) saved
    changed `shouldSatisfy` \case Left (CoreError (JournalFailure (JournalDiverged _))) -> True; _ -> False
    let flow = awaitInt ||| arr id
        pending = onePending (runGuest (resumable guest flow (Left 1)) Map.empty)
        (finished,_,_) = runGuest (resumable guest flow (Right 2)) Map.empty
    pending.request `shouldBe` Integer 1
    finished `shouldBe` Right (RunFinished 2)
  it "round-trips pending data with all schema shapes and rejects malformed descriptors" $ do
    let scalar sh = S.schemaOf sh
        schema = S.Schema (Just "request") (Just "docs") ["check"] (S.SObject
          [S.Field "sum" (scalar (S.SSum [S.Variant "tag" (Just "variant")
             [S.Field "nullable" (scalar (S.SNullable (scalar S.SNumber))) False]])) True
          ,S.Field "array" (scalar (S.SArray (scalar (S.SEnum [("a",Just "doc"),("b",Nothing)])))) True
          ,S.Field "flag" (scalar S.SBool) True
          ,S.Field "null" (scalar S.SNull) True
          ])
        codec = C.Codec schema (Integer . toInteger) intCodec.decode
        w = onePending (runGuest
          (resumable guest (awaitWith codec intCodec `named` "question") 1) Map.empty)
    let address = w.key.key.address
        framed = w{R.key=w.key{J.key=w.key.key{J.context=
          [ElementFrame address 0,IterationFrame address 1,InvocationFrame address 2 3 "provider-id"]}}}
    pendingWaitFromValue (pendingWaitValue framed) `shouldBe` Right framed
    (pendingWaitValue <$> pendingWaitFromValue (pendingWaitValue framed)) `shouldBe` Right (pendingWaitValue framed)
    pendingWaitFromValue (pendingWaitValue w) `shouldBe` Right w
    mapM_ (\format -> do
      let w' = w{replySchema=S.schemaOf (S.SString (Just format))}
      pendingWaitFromValue (pendingWaitValue w') `shouldBe` Right w')
      [S.DateTime,S.Date,S.Email,S.Uri,S.Uuid]
    let extra = case pendingWaitValue w of Object fs -> Object (("extra",Null):fs); _ -> error "expected object"
    pendingWaitFromValue extra `shouldSatisfy` \case Left _ -> True; _ -> False
    pendingWaitFromValue (String "bad") `shouldSatisfy` \case Left _ -> True; _ -> False
    pendingWaitFromValue (Object [("version",Integer 99)]) `shouldSatisfy` \case Left _ -> True; _ -> False
  it "describes await without evaluating codecs or running actions" $ do
    let flow = awaitWith (error "request codec forced") (error "reply codec forced") :: Agentic Guest Int Int
    case describe flow of
      Left e -> expectationFailure (show e)
      Right d -> renderTree d `shouldBe` "await"

  it "distinguishes storage errors from refusals and checks identity before initialise" $ do
    let w = onePending (runGuest (resumable guest awaitInt 1) Map.empty)
        journal = case guest.journal of Just j -> j; Nothing -> error "missing test journal"
        badLookup = guest{journal=Just journal{lookupRecord=const (throwE FetchError)}}
        badSave = guest{journal=Just journal{saveRecord = \_ _ -> throwE FetchError}}
        badInit = guest{journal=Just journal{initialise=throwE FetchError}}
    forM_ [badLookup,badSave,badInit] $ \rt -> do
      let (result,_,_) = runGuest (fulfil rt awaitInt w (Integer 2)) Map.empty
      result `shouldBe` Left FetchError
    let (wrong,_,_) = runGuest (fulfil badInit awaitInt w{R.identity=JournalIdentity "other" "r1"} (Integer 2)) Map.empty
    wrong `shouldBe` Right (Left WaitWrongIdentity)
    let saved = fill awaitInt w (Integer 2) Map.empty
        (changed,_,_) = runGuest (fulfil guest awaitInt w{request=Integer 99} (Integer 2)) saved
    changed `shouldBe` Right (Left ReplyAlreadyAnswered)
  it "resumes loop approval tests before the body, with a fresh reply per pass" $ do
    let flow = repeatUntil (await @Bool @Int)
          (durably (\_ n -> pure (n+1))) :: Agentic Guest Int Int
        attempt = resumable guest flow 0
        first = runGuest attempt Map.empty
        w0 = onePending first
    w0.request `shouldBe` Integer 0
    let (_,empty,_) = first
    Map.null empty `shouldBe` True
    let second@(outcome,saved,events) = runGuest attempt (fill flow w0 (Bool False) empty)
        w1 = onePending second
    outcome `shouldSatisfy` \case Right (RunWaiting _) -> True; _ -> False
    w1.request `shouldBe` Integer 1
    w0.key `shouldNotBe` w1.key
    pendingWaitFromValue (pendingWaitValue w1) `shouldBe` Right w1
    length [() | Executing (KeyedOperation DurableEffect) <- events] `shouldBe` 1
    let (finished,_,restored) = runGuest attempt (fill flow w1 (Bool True) saved)
    finished `shouldBe` Right (RunFinished 1)
    length [() | Restored AwaitedReply <- restored] `shouldBe` 2
    length [() | Restored DurableEffect <- restored] `shouldBe` 1
    length [() | Executing _ <- restored] `shouldBe` 0

  it "finds an await below Second when a transported reply is supplied" $ do
    let flow = second awaitInt :: Agentic Guest (Bool,Int) (Bool,Int)
        w = onePending (runGuest (resumable guest flow (True,1)) Map.empty)
    pendingWaitFromValue (pendingWaitValue w) `shouldBe` Right w
    let (result,_,_) = runGuest (resumable guest flow (True,1)) (fill flow w (Integer 2) Map.empty)
    result `shouldBe` Right (RunFinished (True,2))

  it "collects both halves under split" $ do
    let (outcome,_,_) = runGuest (resumable guest (awaitInt *** awaitInt) (1,2)) Map.empty
    case outcome of
      Right (RunWaiting ws) -> map (.request) (NE.toList ws) `shouldBe` [Integer 1,Integer 2]
      _ -> expectationFailure (show outcome)
