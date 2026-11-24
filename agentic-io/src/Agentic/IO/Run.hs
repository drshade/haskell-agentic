-- | A local host for resumable workflows. Owns the saved input, journal and
-- last successful set of questions. Use one command at a time per directory,
-- including across processes. Like the JSONL journal, this is not a power-loss
-- durable database. The caller supplies a unique run namespace and a revision
-- covering changes to code, types, codecs and model configuration.
module Agentic.IO.Run
  ( Run, runIn, startRun, resumeRun, answerRun, answerRunValue
  , Question (..), questions, asked, RunError (..), AnswerRefused (..), answerRefusalText
  ) where

import Agentic
import Agentic.Aeson (fromAeson)
import Agentic.IO.Journal (withJournal)
import Control.Exception (Exception (..), throwIO)
import Control.Monad (unless, when)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.List (find, nub)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory
import System.FilePath ((</>))

-- | Construct a handle, without opening files or running the flow. The runtime
-- must not already have a journal. Storage is owned by this helper.
data Run i o = Run FilePath JournalIdentity (Runtime IO) (Agentic IO i o) (Codec i)

runIn :: Persist i => FilePath -> JournalIdentity -> Runtime IO -> Agentic IO i o -> Run i o
runIn directory identity rt flow = Run directory identity rt flow persistence

-- | A short run-local ticket and the full question. Tickets are never reused.
data Question = Question {ticket :: Int, pending :: PendingWait}
  deriving (Eq, Show)

-- | Decode the question into the application's request type.
asked :: Persist q => Question -> Either Text q
asked q = persistence.decode q.pending.request

-- | Expected reply refusals. Typed replies are still checked dynamically
-- against the addressed await's codec; choosing the wrong type saves nothing.
data AnswerRefused = QuestionClosed Int | AnswerRejected ReplyRefused
  deriving (Eq, Show)

-- | A short explanation suitable for a CLI or form validation message.
answerRefusalText :: AnswerRefused -> Text
answerRefusalText = \case
  QuestionClosed n -> "Question " <> T.pack (show n) <> " is no longer open."
  AnswerRejected (ReplyInvalid reason) -> "Invalid reply: " <> reason
  AnswerRejected ReplyAlreadyAnswered -> "That question already has a different answer."
  AnswerRejected reason -> "Reply refused: " <> T.pack (show reason)

-- | Storage/configuration failures are exceptions; ordinary answer refusals
-- are returned by answerRun. No error clears the previous successful frontier.
data RunError
  = RunAlreadyExists FilePath
  | RunNotStarted FilePath
  | RunInvalidStore FilePath Text
  | RunIdentityMismatch FilePath
  | RunHasJournal
  deriving (Eq, Show)
instance Exception RunError where
  displayException = \case
    RunAlreadyExists dir -> dir <> " already exists; use resume or choose a new directory"
    RunNotStarted dir -> dir <> " has no saved run; start it first"
    RunInvalidStore dir problem -> dir <> ": " <> T.unpack problem
    RunIdentityMismatch dir -> dir <> ": saved run identity or revision differs"
    RunHasJournal -> "runIn requires a runtime without an installed journal"

data Saved = Saved {input :: Value, nextTicket :: Int, openQuestions :: [Question]}

-- | Start in a new or empty directory. A leftover initial staging file is safe
-- to replace: no workflow has run yet. Existing runs are refused, even if their
-- input matches; use resumeRun instead.
startRun :: Run i o -> i -> IO (Progress o)
startRun run@(Run directory _ _ _ codec) value = do
  checkRuntime run
  exists <- doesDirectoryExist directory
  if exists then do
    entries <- listDirectory directory
    unless (all (== "run.json.tmp") entries) (throwIO (RunAlreadyExists directory))
  else do
    occupied <- doesPathExist directory
    when occupied (throwIO (RunAlreadyExists directory))
  encoded <- canonical run (codec.encode value)
  unless exists (createDirectory directory)
  save run (Saved encoded 0 [])
  resumeRun run

-- | Restore the original input and replay completed work. Only a successful
-- RunWaiting/RunFinished replaces the saved frontier, atomically with tickets.
resumeRun :: Run i o -> IO (Progress o)
resumeRun run@(Run _ _ _ flow codec) = do
  saved <- load run
  value <- either (invalid run) pure (codec.decode saved.input)
  rt <- runtimeFor run
  result <- resumable rt flow value
  let pending = case result of RunFinished _ -> []; RunWaiting ws -> NE.toList ws
      allocate (n,acc) w = case find (\q -> q.pending.key == w.key) saved.openQuestions of
        Just q -> (n,acc <> [q{pending=w}])
        Nothing -> (n+1,acc <> [Question n w])
      (next,qs) = foldl allocate (saved.nextTicket,[]) pending
  when (next < saved.nextTicket) (invalid run "question ticket overflow")
  save run saved{nextTicket=next,openQuestions=qs}
  pure result

-- | Read the last successfully reported questions without running the flow.
questions :: Run i o -> IO [Question]
questions run = (.openQuestions) <$> load run

answerRun :: Persist r => Run i o -> Int -> r -> IO (Either AnswerRefused (Progress o))
answerRun run ticket reply = answerRunValue run ticket (persistence.encode reply)

-- | Validate and save an answer, then resume. A crash after saving the answer
-- is recovered by resumeRun, or by repeating the identical answer. A failure
-- during resume leaves the previous question set in place. Low-level fulfil
-- provides the same immutable, idempotent reply semantics to other hosts.
answerRunValue :: Run i o -> Int -> Value -> IO (Either AnswerRefused (Progress o))
answerRunValue run@(Run _ _ _ flow _) ticket reply = do
  saved <- load run
  case find ((== ticket) . (.ticket)) saved.openQuestions of
    Nothing -> pure (Left (QuestionClosed ticket))
    Just q -> do
      rt <- runtimeFor run
      accepted <- fulfil rt flow q.pending reply
      case accepted of
        Left reason -> pure (Left (AnswerRejected reason))
        Right () -> Right <$> resumeRun run

checkRuntime :: Run i o -> IO ()
checkRuntime (Run _ _ rt _ _) = case rt.journal of
  Nothing -> pure ()
  Just _ -> throwIO RunHasJournal

runtimeFor :: Run i o -> IO (Runtime IO)
runtimeFor run@(Run directory identity rt _ _) = do
  checkRuntime run
  withJournal (directory </> "journal.jsonl") identity rt

invalid :: Run i o -> Text -> IO a
invalid (Run directory _ _ _ _) = throwIO . RunInvalidStore directory

canonical :: Run i o -> Value -> IO Value
canonical run = either (invalid run) pure . canonicalValue

save :: Run i o -> Saved -> IO ()
save run@(Run directory identity _ _ _) saved = do
  value <- canonical run $ Object
    [("version",Integer 1),("run",String identity.namespace),("revision",String identity.revision)
    ,("input",saved.input),("next",Integer (toInteger saved.nextTicket))
    ,("questions",Array [Object [("ticket",Integer (toInteger q.ticket))
        ,("pending",pendingWaitValue q.pending)] | q <- saved.openQuestions])]
  let file = directory </> "run.json"
  BS.writeFile (file <> ".tmp") (TE.encodeUtf8 (renderJson value) <> "\n")
  renameFile (file <> ".tmp") file

load :: Run i o -> IO Saved
load run@(Run directory identity _ _ _) = do
  checkRuntime run
  let file = directory </> "run.json"
  exists <- doesFileExist file
  unless exists (throwIO (RunNotStarted directory))
  bytes <- BS.readFile file
  value <- either (invalid run . T.pack) (canonical run . fromAeson) (JSON.eitherDecodeStrict' bytes)
  fs <- either (invalid run) pure (object value)
  unless (lookupField "run" fs == Just (String identity.namespace)
    && lookupField "revision" fs == Just (String identity.revision))
    (throwIO (RunIdentityMismatch directory))
  either (invalid run) pure $ do
    unless (lookupField "version" fs == Just (Integer 1)) (Left "unsupported run format")
    input <- readField "input" fs
    next <- readField "next" fs >>= index
    qs <- readField "questions" fs >>= \case
      Array xs -> traverse question xs
      _ -> Left "expected questions"
    unless (all (\q -> q.ticket < next && q.pending.identity == identity) qs
      && length (nub (map (.ticket) qs)) == length qs
      && length (nub (map ((.key) . (.pending)) qs)) == length qs)
      (Left "invalid question identities or tickets")
    pure (Saved input next qs)
 where
  question value = do
    fs <- object value
    Question <$> (readField "ticket" fs >>= index) <*> (readField "pending" fs >>= pendingWaitFromValue)
  index (Integer n) | n >= 0, n <= toInteger (maxBound :: Int) = Right (fromInteger n)
  index _ = Left "expected nonnegative ticket"
  object (Object fs) = Right fs
  object _ = Left "expected object"
  readField name = maybe (Left ("missing " <> name)) Right . lookupField name
