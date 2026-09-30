-- | Jev (TypeSafe's System One model) as a runtime's System One.
--
-- > rt <- pure runtime >>= withSystemOne jev
module Agentic.Jev
  ( Jev (..)
  , jev
  , JevError (..)
    -- * Wire format
  , requestBody
  , decodeResponse
  ) where

import Agentic.Aeson (toAeson)
import Agentic.Questions
import Agentic.Runtime (ProvidesSystemOne (..), SystemOne (..))
import Control.Exception (Exception (..), throwIO)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)

-- | Jev's settings. Start from 'jev' and override what you need:
--
-- > jev {jevModel = "jev-1.13.0"}
data Jev = Jev
  { jevModel :: Text
  , jevToken :: Maybe Text
    -- ^ Defaults to the @JEV_TOKEN@ environment variable.
  , jevEndpoint :: String
  , jevTimeoutSeconds :: Int
  }

jev :: Jev
jev =
  Jev
    { jevModel = "jev-latest"
    , jevToken = Nothing
    , jevEndpoint = "https://api.typesafe.ai/v1/systemone"
    , jevTimeoutSeconds = 30
    }

data JevError
  = MissingToken
  | Rejected Int Text
    -- ^ Jev answered with a non-200 status, and this body.
  | UnexpectedResponse Text
  deriving (Show)

instance Exception JevError where
  displayException = \case
    MissingToken -> "Jev: no token. Set JEV_TOKEN, or jevToken in the config."
    Rejected status body -> "Jev rejected the request (HTTP " <> show status <> "): " <> T.unpack body
    UnexpectedResponse problem -> "Jev sent a response agentic can't read: " <> T.unpack problem

instance ProvidesSystemOne Jev where
  toSystemOne cfg = do
    token <- maybe (fmap T.pack <$> lookupEnv "JEV_TOKEN") (pure . Just) (jevToken cfg)
    key <- maybe (throwIO MissingToken) pure token
    manager <- newTlsManager
    base <- Http.parseRequest (jevEndpoint cfg)
    pure $ SystemOne $ \request -> do
      let http =
            base
              { Http.method = "POST"
              , Http.requestHeaders =
                  [ ("Authorization", "Bearer " <> T.encodeUtf8 key)
                  , ("Content-Type", "application/json")
                  ]
              , Http.requestBody = Http.RequestBodyLBS (J.encode (requestBody (jevModel cfg) request))
              , Http.responseTimeout = Http.responseTimeoutMicro (jevTimeoutSeconds cfg * 1000000)
              }
      response <- Http.httpLbs http manager
      let status = statusCode (Http.responseStatus response)
          body = Http.responseBody response
      if status /= 200
        then throwIO (Rejected status (T.decodeUtf8Lenient (LBS.toStrict body)))
        else case J.eitherDecode body of
          Left problem -> throwIO (UnexpectedResponse (T.pack problem))
          Right value -> either (throwIO . UnexpectedResponse) pure (decodeResponse request value)

-- | The request body: the state, and each question under an id (@q0@, @q1@, …).
requestBody :: Text -> JudgeRequest -> J.Value
requestBody model request =
  J.object
    [ "model" .= model
    , "state" .= toAeson (requestState request)
    , "questions" .= J.object [Key.fromText qid .= question q | (qid, q) <- ided (requestQuestions request)]
    ]
  where
    question = \case
      AskYesNo q -> J.object ["type" .= ("noul" :: Text), "instructions" .= q]
      AskChoice q opts ->
        J.object
          [ "type" .= ("choice" :: Text)
          , "instructions" .= q
          , "criteria" .= J.object [Key.fromText l .= d | (l, d) <- opts]
          ]
      AskScore q levels ->
        J.object
          [ "type" .= ("score" :: Text)
          , "instructions" .= q
          , "criteria" .= [maybe l id d | (l, d) <- levels]
          ]

-- | Read Jev's answers back, in question order.
decodeResponse :: JudgeRequest -> J.Value -> Either Text [Answer]
decodeResponse request = either (Left . T.pack) Right . J.parseEither parse
  where
    parse = J.withObject "response" $ \response -> do
      answers <- response .: "answers"
      traverse (\(qid, q) -> answers .: Key.fromText qid >>= answer q) (ided (requestQuestions request))
    answer q = J.withObject "answer" $ \a -> case q of
      AskYesNo _ -> YesNoAnswer . fromBasisPoints <$> a .: "noul"
      AskChoice _ opts -> do
        ps <- a .: "probabilities"
        ChoiceAnswer
          <$> a .: "choice"
          <*> traverse (\(l, _) -> (l,) . fromBasisPoints <$> ps .: Key.fromText l) opts
          <*> (fromBasisPoints <$> a .: "confidence")
      AskScore _ levels -> do
        ps <- a .: "probabilities"
        ScoreAnswer
          <$> a .: "score"
          <*> traverse (\i -> (i,) . fromBasisPoints <$> ps .: Key.fromText (T.pack (show i))) [0 .. length levels - 1]
          <*> (fromBasisPoints <$> a .: "confidence")

ided :: [a] -> [(Text, a)]
ided = zip ["q" <> T.pack (show n) | n <- [0 :: Int ..]]
