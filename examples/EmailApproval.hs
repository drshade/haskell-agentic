-- | Draft, review, revise and approve an email across separate invocations.
-- No email is delivered. Default drafting is deterministic; --live uses Claude.
module EmailApproval where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.Aeson (fromAeson)
import Agentic.Codec qualified as C
import Agentic.IO (loadDotEnv, withJournal)
import Agentic.Runtime (Conversation (..))
import Agentic.Schema (Shape (..), Field (..))
import Agentic.Scripted (modelContract, respond)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (unless, when)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import GHC.Generics (Generic)
import System.Directory
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Error (ioeGetErrorString)
import System.FilePath ((</>))
import Text.Read (readMaybe)

data Email = Email {subject :: Text, body :: Text}
  deriving (Eq, Show, Generic, Contract)

data DraftRequest = DraftRequest
  { originalBrief :: Text
  , previousDraft :: Maybe Email
  , changes :: Text
  } deriving (Eq, Show, Generic, Contract)

data ApprovalRequest = ApprovalRequest
  { revision :: Int, originalBrief :: Text, email :: Email }
  deriving (Eq, Show)

data Decision = Approve | RequestChanges Text | Reject
  deriving (Eq, Show)

data Delivery = EmailSent Email | EmailRejected
  deriving (Eq, Show)

type Sender = Maybe DurableKey -> Email -> IO ()

-- The first draft is a saved model turn. Each revision is another turn at a
-- different loop occurrence; answering an old wait never approves a new draft.
emailApproval :: Sender -> Agentic IO Text Delivery
emailApproval send =
  arr (\brief -> DraftRequest brief Nothing "")
    >>> (returnA &&& writeEmail)
    >>> arr (\(request,mail) -> ApprovalRequest 0 request.originalBrief mail)
          `named` "prepare review"
    >>> reviewEmail
    >>> repeatUntil decided reviseAndReview
    >>> arr route
    >>> ((durably emailCodec emailCodec (\stableKey mail -> send stableKey mail >> pure mail)
            `named` "print approved email" >>> arr EmailSent)
          ||| (arr (const EmailRejected) `named` "reject email")) `named` "approved?"
 where
  writeEmail = draft @Email
    "Draft the email. Apply any requested changes to the previous draft."
    `named` "draft email"
  reviewEmail = returnA &&& (await approvalCodec decisionCodec
    `named` "approve, revise or reject")
  reviseAndReview =
    arr (\(request,decision) -> (request,nextDraft request decision))
      >>> (takeFirst &&& (takeSecond >>> writeEmail))
      >>> arr (\(request,mail) ->
        ApprovalRequest (request.revision+1) request.originalBrief mail)
            `named` "prepare revised review"
      >>> reviewEmail
  decided (_,RequestChanges _) = False
  decided _ = True
  -- Only RequestChanges reaches this body, because repeatUntil checks first.
  nextDraft request decision = DraftRequest request.originalBrief
    (Just request.email) (case decision of RequestChanges edits -> edits; _ -> "")
  route (request,Approve) = Left request.email
  route _ = Right ()

-- Explicit persistence codecs: these save every field of the request/reply.
textCodec :: C.Codec Text
textCodec = C.primitive (SString Nothing) String $ \case
  String t -> Right t
  _ -> Left "expected text"

intCodec :: C.Codec Int
intCodec = C.primitive SInteger (Integer . toInteger) $ \case
  Integer n | n >= 0, n <= toInteger (maxBound :: Int) -> Right (fromInteger n)
  _ -> Left "expected a nonnegative Int"

emailCodec :: C.Codec Email
emailCodec = C.record "Email" $ Email
  <$> C.requiredWith "subject" Nothing textCodec (.subject)
  <*> C.requiredWith "body" Nothing textCodec (.body)

approvalCodec :: C.Codec ApprovalRequest
approvalCodec = C.record "Email to review" $ ApprovalRequest
  <$> C.requiredWith "revision" Nothing intCodec (.revision)
  <*> C.requiredWith "originalBrief" Nothing textCodec (.originalBrief)
  <*> C.requiredWith "email" Nothing emailCodec (.email)

decisionCodec :: C.Codec Decision
decisionCodec = C.sumOf "Approval decision"
  [ C.CaseCodec "approve" Nothing []
      (\case Approve -> Just []; _ -> Nothing) (const (Right Approve))
  , C.CaseCodec "reject" Nothing []
      (\case Reject -> Just []; _ -> Nothing) (const (Right Reject))
  , C.CaseCodec "revise" Nothing [Field "changes" textCodec.schema True]
      (\case RequestChanges edits -> Just [("changes",String edits)]; _ -> Nothing)
      (\fs -> case lookupField "changes" fs of
        Just (String edits) | not (T.null (T.strip edits)) -> Right (RequestChanges edits)
        _ -> Left "describe the changes you want")
  ]

-- An intentionally simple, deterministic provider for trying the CLI without
-- credentials. In a revision it quotes the requested changes; it is not an LLM.
demoModel :: SystemTwo IO
demoModel = SystemTwo $ \conversation -> do
  request <- either (fail . T.unpack) pure ((modelContract @DraftRequest).decode conversation.input)
  T.putStrLn "[demo] Drafting email."
  let mail = case request.previousDraft of
        Nothing -> Email "A quick update"
          ("Hello,\n\n" <> request.originalBrief <> "\n\nBest,\nThe team")
        Just previous -> Email previous.subject
          (previous.body <> "\n\nRequested changes: " <> request.changes)
  pure (Turn (Raw Null) (respond mail))

fakeSend :: Sender
fakeSend stableKey mail = do
  sendKey <- maybe (fail "the fake sender expects a journaled run") pure stableKey
  T.putStrLn "FAKE SEND (no email delivered)"
  T.putStrLn ("Idempotency key: " <> durableKeyText sendKey)
  printEmail mail

printEmail :: Email -> IO ()
printEmail mail = T.putStrLn ("Subject: " <> mail.subject <> "\n\n" <> mail.body)

-- The host owns the run's stable root input and pending descriptors. Nothing
-- here waits on stdin. Use one command at a time for a given directory.
data RunConfig = RunConfig {brief :: Text, live :: Bool, namespace :: Text}
  deriving (Eq, Show)

writeValue :: FilePath -> Value -> IO ()
writeValue file value = do
  BS.writeFile (file <> ".tmp") (T.encodeUtf8 (renderJson value) <> "\n")
  renameFile (file <> ".tmp") file

readValue :: FilePath -> IO Value
readValue file = BS.readFile file >>= either fail (pure . fromAeson) . JSON.eitherDecodeStrict'

readConfig :: FilePath -> IO RunConfig
readConfig directory = readValue (directory </> "run.json") >>= \case
  Object fs | Just (String brief) <- lookupField "brief" fs
            , Just (Bool live) <- lookupField "live" fs
            , Just (String namespace) <- lookupField "run" fs ->
                pure (RunConfig brief live namespace)
  _ -> fail "invalid run.json"

openRun :: FilePath -> RunConfig -> IO (Runtime IO)
openRun directory config = do
  let revision = if config.live then "email-approval-v1-claude" else "email-approval-v1-demo"
  withJournal (directory </> "journal.jsonl")
    (JournalIdentity config.namespace revision) runtime

attempt :: FilePath -> IO (Progress Delivery)
attempt directory = do
  config <- readConfig directory
  rt <- openRun directory config
  drafting <- if config.live
    then loadDotEnv >> withSystemTwo (anthropic & effort Low) rt
    else pure rt{systemTwo=demoModel}
  result <- resumable drafting (emailApproval fakeSend) config.brief
  case result of
    RunWaiting (pending NE.:| []) -> do
      revision <- showQuestion directory pending
      writeValue (directory </> "state.json") (Object [("pending",Integer (toInteger revision))])
    RunWaiting _ -> fail "this example expects one approval at a time"
    RunFinished delivery -> do
      writeValue (directory </> "state.json") (Object [("pending",Null)])
      putStrLn $ case delivery of
        EmailSent _ -> "Done: approved email sent by the fake sender."
        EmailRejected -> "Done: rejected; nothing sent."
  pure result

pendingFile :: FilePath -> Int -> FilePath
pendingFile directory revision = directory </> ("pending-" <> show revision <> ".json")

showQuestion :: FilePath -> PendingWait -> IO Int
showQuestion directory pending = do
  question <- either (fail . T.unpack) pure (approvalCodec.decode pending.request)
  let file = pendingFile directory question.revision
  writeValue file (pendingWaitValue pending)
  putStrLn ("Awaiting approval for draft " <> show question.revision <> ".")
  printEmail question.email
  putStrLn ("Saved question: " <> file)
  putStrLn "Reply with its draft number: approve, revise <feedback>, or reject."
  pure question.revision

start :: FilePath -> Bool -> Text -> IO (Progress Delivery)
start directory live brief = do
  when (T.null (T.strip brief)) (fail "provide a nonempty email brief")
  -- Refuse existing directories instead of replacing an earlier run.
  exists <- doesDirectoryExist directory
  when exists (fail (directory <> " already exists; start needs a new directory"))
  createDirectory directory
  -- A real host would mint a unique run id; a directory path suffices only for this local demo.
  namespace <- T.pack <$> canonicalizePath directory
  writeValue (directory </> "run.json") (Object
    [("brief",String brief),("live",Bool live),("run",String namespace)])
  attempt directory

reply :: FilePath -> Int -> Decision -> IO (Progress Delivery)
reply directory revision decision = do
  state <- readValue (directory </> "state.json")
  let isOpen = case state of
        Object fs -> lookupField "pending" fs == Just (Integer (toInteger revision))
        _ -> False
  unless isOpen $ fail ("Draft " <> show revision
    <> " is no longer awaiting approval. Use resume to see the current state.")
  config <- readConfig directory
  rt <- openRun directory config
  pending <- readValue (pendingFile directory revision)
    >>= either (fail . T.unpack) pure . pendingWaitFromValue
  accepted <- fulfil rt (emailApproval fakeSend) pending (decisionCodec.encode decision)
  case accepted of
    Left refused -> fail ("Reply refused: " <> show refused)
    Right () -> attempt directory

main :: IO ()
main = runCLI `catch` report
 where
  report :: IOException -> IO ()
  report = die . ioeGetErrorString

runCLI :: IO ()
runCLI = getArgs >>= \case
  ["--mermaid"] -> either throwIO (T.putStrLn . mermaid) (describe (emailApproval fakeSend))
  ["start",directory,brief] -> start directory False (T.pack brief) >> pure ()
  ["start",directory,"--demo",brief] -> start directory False (T.pack brief) >> pure ()
  ["start",directory,"--live",brief] -> start directory True (T.pack brief) >> pure ()
  ["resume",directory] -> attempt directory >> pure ()
  ["reply",directory,number,"approve"] -> answer directory number Approve
  ["reply",directory,number,"reject"] -> answer directory number Reject
  ["reply",directory,number,"revise",edits] ->
    answer directory number (RequestChanges (T.pack edits))
  _ -> fail "usage: email-approval start DIR [--demo|--live] BRIEF | resume DIR | \
    \reply DIR N approve|reject|revise FEEDBACK | --mermaid"
 where
  answer directory number decision = do
    revision <- maybe (fail "draft number must be a nonnegative Int") pure (readMaybe number)
    unless (revision >= 0) (fail "draft number must be a nonnegative Int")
    _ <- reply directory revision decision
    pure ()
