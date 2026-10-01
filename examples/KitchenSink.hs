-- | Every feature of the library in one flow: a day of support email at an
-- online bookshop.
--
-- Claude writes the day's inbox. Jev drops the spam and triages each email
-- (topic, urgency and mood, in one request), and code turns that into a ticket.
-- Urgent tickets and routine ones are handled side by side: refunds go to an
-- agent with tools, everything else gets a plain reply, every reply is
-- polished until Jev rates it polite, and each ticket gets a log line. Urgent
-- tickets also page the on-call team. Claude ends the day with a report.
--
-- Runs with Claude and Jev; needs ANTHROPIC_API_KEY and JEV_TOKEN, in the
-- environment or .env. Model calls are recorded to kitchensink.jsonl, so a
-- second run replays them in a moment. (With no Jev token, an LLM can answer
-- the judgements instead: withSystemOne anthropic.)
module Main (main) where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.IO (Mode (..), concurrently, loadDotEnv, withStore)
import Agentic.Jev (jev)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- What comes in

data Email = Email {sender :: Text, subject :: Text, message :: Text}
  deriving (Generic, Show)

instance Contract Email where
  contract =
    record "An email to the shop's support address" $
      Email
        <$> required "sender" "The customer's email address" sender
        <*> required "subject" "The subject line" subject
        <*> required "message" "The body of the email" message

-- ---------------------------------------------------------------------------
-- Triage: Jev judges, code decides

data Topic = Refund | Delivery | Recommendation | Complaint | Other
  deriving (Generic, Show, Eq)

instance Options Topic where
  options =
    described
      "What the email is about"
      [ option Refund "Wants their money back for an order"
      , option Delivery "Asks where an order is, or reports a delivery problem"
      , option Recommendation "Wants a book suggested"
      , option Complaint "Unhappy about the shop or its service, without asking for a refund"
      , option Other "Anything else"
      ]

deriving via Enumeration Topic instance Contract Topic

data Urgency = Routine | Soon | Urgent
  deriving (Generic, Show, Eq)

instance Options Urgency where
  options =
    described
      "How quickly the shop should answer"
      [ option Routine "Can wait a day or two"
      , option Soon "Should be answered today"
      , option Urgent "Needs an answer within the hour"
      ]

deriving via Enumeration Urgency instance Contract Urgency

-- | What Jev says about an email: three questions, one request.
data Triage = Triage {topic :: Choice Topic, urgency :: Score Urgency, angry :: YesNo}

triageQuestions :: Questions Triage
triageQuestions =
  Triage
    <$> choice "What is this email about?"
    <*> score "How urgent is this email?"
    <*> yesNo "Is the customer angry?"

-- | What the shop decides, from Jev's answers.
data Ticket = Ticket {email :: Email, about :: Topic, urgent :: Bool, upset :: Bool}
  deriving (Generic, Show, Contract)

decide :: (Email, Triage) -> Ticket
decide (e, t) = Ticket e (chosen (topic t)) (position (urgency t) >= 1.5 || isUpset) isUpset
  where
    isUpset = yes (angry t) >= 0.5

-- ---------------------------------------------------------------------------
-- What goes out

-- | A subject line. The contract tells the model the limit and checks it.
newtype Subject = Subject Text
  deriving (Show)

instance Contract Subject where
  contract =
    documented "A subject line for the reply" $
      checked "60 characters or fewer" ((<= 60) . T.length . (\(Subject s) -> s)) $
        mapCodec Subject (\(Subject s) -> s) contract

data Reply = Reply {recipient :: Text, subjectLine :: Subject, text :: Text}
  deriving (Generic, Show, Contract)

data Handled = Handled {ticket :: Ticket, reply :: Reply, logEntry :: Text}
  deriving (Generic, Show, Contract)

data Day = Day {received :: Int, urgentTickets :: [Handled], routineTickets :: [Handled]}
  deriving (Generic, Show, Contract)

data Report = Report {headline :: Text, summary :: Text}
  deriving (Generic, Show, Contract)

-- ---------------------------------------------------------------------------
-- The refund agent's tools

data OrderQuery = OrderQuery {customer :: Text}
  deriving (Generic, Show, Contract)

data Order = Order {book :: Text, purchasedDaysAgo :: Int}
  deriving (Generic, Show, Contract)

-- | A stand-in for the shop's order database: every customer has one order,
-- worked out from their address.
lookupOrder :: Tool IO
lookupOrder =
  tool @OrderQuery @Order "lookup_order" "Look up a customer's most recent order by their email address" $
    act $ \(OrderQuery c) ->
      let n = T.length c
       in pure (Order (["Dune", "Middlemarch", "The Hobbit", "Beloved"] !! (n `mod` 4)) (n * 3 `mod` 45))

data RefundCase = RefundCase {reason :: Text, daysSincePurchase :: Int}
  deriving (Generic, Show)

instance Contract RefundCase where
  contract =
    record "A refund request" $
      RefundCase
        <$> required "reason" "Why the customer wants a refund" reason
        <*> required "daysSincePurchase" "Days since the order was placed" (\(RefundCase _ d) -> d)

-- | Jev, as a tool: does a refund request fit the policy?
refundEligible :: Tool IO
refundEligible =
  tool @RefundCase @YesNo "refund_eligible" "Check a refund request against the shop's refund policy" $
    judge (yesNo "Our policy: refunds within 30 days of purchase, for damaged books or books that weren't what was ordered. Does this request fit the policy?")

-- ---------------------------------------------------------------------------
-- The flow

kitchenSink :: Agentic IO () Report
kitchenSink =
  draft @[Email] "Write 8 emails to the support address of an online bookshop, as they might arrive in a day. Include one spam email, one refund request and one angry customer."
    >>> (arr length `named` "count the emails" &&& keep 0.5 (yesNo "Is this a genuine email from a customer, not spam?"))
    >>> second (each triage)
    >>> second (arr (partition urgent) `named` "split off the urgent tickets")
    >>> second (each (handle >>> act page `named` "page the on-call team") *** each handle)
    >>> arr (\(n, (u, r)) -> Day n u r)
    >>> draft @Report "Summarise the day's support email for the support lead."

triage :: Agentic IO Email Ticket
triage =
  (returnA &&& judge triageQuestions)
    >>> note "triage policy" "Urgent if Jev scores it above Soon, or the customer is angry" (arr decide)

-- | Reply, polish, send, and log it.
handle :: Agentic IO Ticket Handled
handle =
  ((route >>> polish) &&& draft @Text "Write a one-line log entry for this support ticket.")
    >>> arr (\((t, r), l) -> Handled t r l)
    >>> act send `named` "send the reply"

route :: Agentic IO Ticket (Ticket, Reply)
route =
  arr (\t -> if about t == Refund then Left t else Right t) `named` "refunds to the refund agent"
    >>> (returnA &&& refundAgent ||| returnA &&& draft @Reply "Write a helpful reply to this customer's email.")

refundAgent :: Agentic IO Ticket Reply
refundAgent =
  draftWith @Reply
    [lookupOrder, refundEligible]
    "This customer wants a refund. Look up their order, check the request against the refund policy, and write a reply that tells them the outcome."

-- | Revise the reply until Jev rates it polite.
polish :: Agentic IO (Ticket, Reply) (Ticket, Reply)
polish =
  note "polish" "Revise until Jev rates the reply polite (≥ 0.9)" $
    rate
      >>> repeatUntil ((>= 0.9) . yes . snd) (arr fst >>> second (draft @Reply "Make this reply warmer and more polite, keeping what it says.") >>> rate)
      >>> arr fst
  where
    rate = returnA &&& (arr (text . snd) >>> judge (yesNo "Is this reply polite and warm?"))

send :: Handled -> IO Handled
send h = do
  let Reply to (Subject s) _ = reply h
  T.putStrLn ("  sent to " <> to <> ": " <> s)
  pure h

page :: Handled -> IO Handled
page h = T.putStrLn ("  paged on-call about: " <> subject (email (ticket h))) >> pure h

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  _ <- loadDotEnv
  print $ describe kitchenSink
  T.putStrLn $ "\n" <> mermaid (describe kitchenSink)
  T.putStrLn $ dot (describe kitchenSink)
  rt <-
    pure (concurrently runtime)
      >>= withSystemOne jev
      >>= withSystemTwo (anthropic & effort Low)
      >>= withStore ReplayOrRecord "kitchensink.jsonl"
  report <- interpret rt kitchenSink ()
  T.putStrLn $ "\n" <> headline report <> "\n\n" <> summary report
