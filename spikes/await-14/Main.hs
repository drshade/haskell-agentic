{-# LANGUAGE GHC2021, DuplicateRecordFields, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings, LambdaCase #-}
module Main where

import Agentic hiding (key, Tool)
import Agentic.Codec qualified as C
import Agentic.Schema (Shape (..))
import Agentic.Runtime (Conversation (..))
import Agentic.Internal.SpikeCommon
import Agentic.Internal.SpikeExplicit qualified as E
import Agentic.Internal.SpikeTransformer qualified as T
import Agentic.IO (concurrently)
import Control.Concurrent.MVar
import Control.Concurrent.Async qualified as Async
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.IORef
import GHC.Clock
import GHC.Stats
import System.Mem (performGC)
import System.Environment (getArgs)
import Text.Printf

type Store = Map.Map Value Value
newtype Runner = Runner {run :: forall m i o. Monad m => Runtime m -> Agentic m i o -> i -> m (Either Waits o)}
explicit, transformer, baseline, gatedE, gatedT :: Runner
explicit = Runner E.interpret
transformer = Runner T.interpret
baseline = Runner (\rt f x -> Right <$> interpret rt f x)
gatedE = Runner (\rt f x -> case rt.journal of Nothing -> Right <$> interpret rt f x; Just _ -> E.interpret rt f x)
gatedT = Runner (\rt f x -> case rt.journal of Nothing -> Right <$> interpret rt f x; Just _ -> T.interpret rt f x)
runWith :: Monad m => Runner -> Runtime m -> Agentic m i o -> i -> m (Either Waits o)
runWith (Runner f) = f

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
      (\k v -> ExceptT (Get (\s -> Put (Map.insert k v s) (Done (Right ())))))) }
runGuest :: Guest a -> Store -> (Either FetchError a, Store, [Happened])
runGuest = runTool . runExceptT

-- Spike helper only: caller supplies the real codec. Structural codec lookup
-- and the public fulfil API are intentionally deferred to full implementation.
supply :: C.Codec a -> Pending -> Value -> Store -> Either Text Store
supply codec (Pending _ k request) reply store = do
  v <- canonicalValue reply
  _ <- codec.decode v
  req <- canonicalValue request
  jk <- canonicalValue (journalKeyValue k)
  let record = Object [("request",req),("result",v)]
  pure (Map.insert jk record store)

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

behaviour :: String -> Runner -> IO ()
behaviour name runner = do
  let run :: Monad m => Runtime m -> Agentic m i o -> i -> m (Either Waits o)
      run = runWith runner
      (first,s,events) = runGuest (run guest (each awaitInt) [10,20,30]) Map.empty
  waits <- case first of Right (Left ws) -> pure ws; _ -> error (show first)
  check "three reached waits" (NE.length waits == 3 && length [() | Suspended <- events] == 5)
  check "nothing saved on pending" (Map.null s)
  let (w1,w2,w3) = case NE.toList waits of [a,b,c] -> (a,b,c); _ -> error "expected three waits"
      fill w v st = either (error . show) id (supply intCodec w v st)
      s1 = fill w1 (Number 1.0) s
      (second,_,_) = runGuest (run guest (each awaitInt) [10,20,30]) s1
  check "one reply leaves two waits" (case second of Right (Left ws) -> NE.length ws == 2; _ -> False)
  let s2 = fill w3 (Integer 3) (fill w2 (Integer 2) s1)
      (third,_,_) = runGuest (run guest (each awaitInt) [10,20,30]) s2
  check "reply reached next step" (third == Right (Right [1,2,3]))
  check "bad reply rejected" (case supply intCodec w1 (String "bad") s of Left _ -> True; _ -> False)
  let (failure,_,failureEvents) = runGuest (run guest
        (awaitInt &&& act (const (throwE FetchError :: Guest Int))) 10) Map.empty
  check "failure stays separate" (failure == Left FetchError && Failed `elem` failureEvents)
  let observerRt = guest{observe = \e -> if e.happened == Suspended then throwE FetchError else guest.observe e}
      (observerFailure,_,obsEvents) = runGuest (run observerRt awaitInt 1) Map.empty
  check "observer failure wins" (observerFailure == Left FetchError && Failed `elem` obsEvents)
  let (seqResult,_,_) = runGuest (run guest (awaitInt >>> awaitInt) 10) Map.empty
  check "sequence frontier" (case seqResult of Right (Left ws) -> NE.length ws == 1; _ -> False)
  let flow = awaitInt &&& durably intCodec intCodec (\_ -> pure . (+1))
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
  let (rec2,rs2,_) = resumeRec (fill rw1 (Integer 0) rs1)
  rw2 <- case rec2 of Right (Left (w NE.:| [])) -> pure w; _ -> error (show rec2)
  check "recursive invocations have distinct wait keys" (rw1 /= rw2)
  let (rec3,_,_) = resumeRec (fill rw2 (Integer 1) rs2)
  check "recursive tool resumes" (rec3 == Right (Right 2))
  let toolRt = guest{systemTwo=SystemTwo (\c -> pure $ if null c.history
        then Turn (Raw (String "batch")) (CallTools
          [ToolCall "same" "ask" (Integer 1),ToolCall "same" "ask" (Integer 2)])
        else Turn (Raw (String "done")) (Respond (Integer 3)))}
      toolFlow = draftWith [tool @Int @Int "ask" "" awaitInt] "ask"
        :: Agentic Guest Int Int
      (toolWaits,_,_) = runGuest (run toolRt toolFlow 0) Map.empty
  check "tool batch exposes both waits" (case toolWaits of Right (Left ws) -> NE.length ws == 2; _ -> False)
  -- Test actual IO observer cancellation: cancellation remains an exception,
  -- never converted into Waiting, and the parent records Cancelled.
  rt <- ioJournal False
  seen <- newIORef []
  let cancelRt = rt{observe = \e -> do
        modifyIORef' seen (e.happened:)
        when (e.happened == Suspended && e.nodeType == EffectNode) (throwIO Async.AsyncCancelled)}
  cancelled <- try (run cancelRt awaitInt 2)
  check "IO observer cancellation propagates" (case cancelled of Left Async.AsyncCancelled -> True; _ -> False)
  observed <- readIORef seen
  check "parent cancellation classified" (Cancelled `elem` observed)
  -- The first child suspends before the durable sibling is released. The
  -- sibling must still execute; no exception channel cancels it on a wait.
  gate <- newEmptyMVar
  hit <- newIORef (0::Int)
  let parRt = (concurrently rt){observe = \e -> when (e.happened == Suspended && e.nodeType == EffectNode) (void (tryPutMVar gate ()))}
      durable = durably intCodec intCodec (\_ x -> readMVar gate >> modifyIORef' hit (+1) >> pure x)
  both <- run parRt (awaitInt &&& durable) 8
  n <- readIORef hit
  check "concurrent sibling completes" (n == 1 && case both of Left _ -> True; _ -> False)
  putStrLn (name <> ": pure Program + ExceptT, frontier, codec normalisation, sibling save, observer failure/cancellation passed")

ioJournal :: Bool -> IO (Runtime IO)
ioJournal parallel = do
  cell <- newMVar Map.empty
  let j = Journal ident (pure ())
        (\k -> withMVar cell (pure . Map.lookup k))
        (\k v -> modifyMVar_ cell (pure . Map.insert k v))
      rt = runtime{journal=Just j}
  pure (if parallel then concurrently rt else rt)

cheap :: Agentic IO [Int] [Int]
cheap = each ((arr (+1) `named` "increment") >>> arr (*2))
durableFlow :: Agentic IO [Int] [Int]
durableFlow = each (durably intCodec intCodec (\_ -> pure . (+1)))

type BenchRunner = Runtime IO -> Agentic IO [Int] [Int] -> [Int] -> IO (Either Waits [Int])

measure :: String -> [(String, BenchRunner)] -> IO (Runtime IO) -> Agentic IO [Int] [Int] -> IO ()
measure label runners setup flow = do
  -- Each sample prepares outside the measured region. Rotate runner order to
  -- avoid always favouring the first representation. Force the final result.
  samples <- forM [0..6::Int] $ \roundNo -> do
    let order = drop (roundNo `mod` length runners) runners <> take (roundNo `mod` length runners) runners
    forM order $ \(name,runner) -> do
      rt <- setup
      performGC
      before <- getRTSStats
      start <- getMonotonicTimeNSec
      out <- runner rt flow [1..5000]
      _ <- evaluate (case out of Right xs -> sum xs; Left ws -> NE.length ws)
      end <- getMonotonicTimeNSec
      performGC
      after <- getRTSStats
      pure (name,fromIntegral (end-start)/1000000::Double,allocated_bytes after-allocated_bytes before)
  forM_ runners $ \(name,_) -> do
    let times = sort [t | xs <- samples,(n,t,_) <- xs,n == name]
        bytes = sort [fromIntegral b / 1000000::Double | xs <- samples,(n,_,b) <- xs,n == name]
    printf "%s,%s,%.3f,%.3f\n" label name (times !! 3) (bytes !! 3)

benchmarks :: IO ()
benchmarks = do
  let mainRun :: BenchRunner
      mainRun rt f x = Right <$> interpret rt f x
      runners = [("main",mainRun),("explicit",E.interpret),("transformer",T.interpret)]
      gateE rt f x = case rt.journal of Nothing -> mainRun rt f x; Just _ -> E.interpret rt f x
      gateT rt f x = case rt.journal of Nothing -> mainRun rt f x; Just _ -> T.interpret rt f x
  putStrLn "case,representation,median_ms,allocated_MB (5000 elements, 7 rotated samples, GHC -O1, +RTS -T)"
  measure "no-journal-raw" runners (pure runtime) cheap
  measure "no-journal-gated" [("main",mainRun),("explicit-gated",gateE),("transformer-gated",gateT)] (pure runtime) cheap
  forM_ [False,True] $ \par -> do
    let tag = if par then "concurrent" else "sequential"
        fresh = ioJournal par
        warm = do
          rt <- fresh
          _ <- interpret rt durableFlow [1..5000]
          pure rt
    measure (tag<>"-journal-cheap") runners fresh cheap
    measure (tag<>"-durable-miss") runners fresh durableFlow
    measure (tag<>"-durable-hit") runners warm durableFlow
    measure (tag<>"-waiting") (drop 1 runners) fresh (each awaitInt)

main :: IO ()
main = getArgs >>= \case
  ["bench"] -> benchmarks
  _ -> behaviour "explicit" explicit >> behaviour "transformer" transformer
