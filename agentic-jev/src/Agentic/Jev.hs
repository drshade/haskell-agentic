-- | Jev (TypeSafe's System One model) as a runtime's System One.
--
-- > rt <- pure runtime >>= withSystemOne jev
-- > rt <- pure runtime >>= withSystemOne (jev & model "jev-1.13.0")
module Agentic.Jev
  ( Jev (..)
  , jev
  , JevError (..)
    -- * Wire format
  , requestBody
  , decodeResponse
  ) where

import qualified Agentic.Value as A
import Agentic.Questions
import Agentic.Runtime (ProvidesSystemOne (..), SystemOne (..))
import Agentic.Settings
import Control.Exception (Exception (..), throwIO)
import Data.Aeson ((.:))
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

-- | Jev's settings. Start from 'jev' and change them with the setters from
-- "Agentic.Settings": 'Agentic.Settings.model', 'Agentic.Settings.key',
-- 'Agentic.Settings.endpoint' and 'Agentic.Settings.timeout'.
data Jev = Jev
  { model :: Text
  , key :: Maybe Text
    -- ^ Defaults to the @JEV_TOKEN@ environment variable.
  , endpoint :: String
  , timeout :: Int
    -- ^ Seconds.
  }

jev :: Jev
jev =
  Jev
    { model = "jev-latest"
    , key = Nothing
    , endpoint = "https://api.typesafe.ai/v1/systemone"
    , timeout = 30
    }

instance HasModel Jev where model m c = c {model = m}
instance HasKey Jev where key k c = c {key = Just k}
instance HasEndpoint Jev where endpoint e c = c {endpoint = e}
instance HasTimeout Jev where timeout t c = c {timeout = t}

-- | What can go wrong talking to Jev. Thrown in IO.
data JevError
  = MissingToken
  | HttpError Int Text
    -- ^ Jev answered with a non-200 status, and this body.
  | UnexpectedResponse Text
  deriving (Show)

instance Exception JevError where
  displayException = \case
    MissingToken -> "Jev: no token. Set JEV_TOKEN, or use (jev & key ...)."
    HttpError status body -> "Jev rejected the request (HTTP " <> show status <> "): " <> T.unpack body
    UnexpectedResponse problem -> "Jev sent a response agentic can't read: " <> T.unpack problem

instance ProvidesSystemOne Jev where
  toSystemOne cfg = do
    token <- maybe (fmap T.pack <$> lookupEnv "JEV_TOKEN") (pure . Just) cfg.key
    token' <- maybe (throwIO MissingToken) pure token
    manager <- newTlsManager
    base <- Http.parseRequest cfg.endpoint
    pure $ SystemOne $ \request -> do
      let http =
            base
              { Http.method = "POST"
              , Http.requestHeaders =
                  [ ("Authorization", "Bearer " <> T.encodeUtf8 token')
                  , ("Content-Type", "application/json")
                  ]
              , Http.requestBody = Http.RequestBodyBS (T.encodeUtf8 (A.renderJson (requestBody cfg.model request)))
              , Http.responseTimeout = Http.responseTimeoutMicro (cfg.timeout * 1000000)
              }
      response <- Http.httpLbs http manager
      let status = statusCode (Http.responseStatus response)
          body = Http.responseBody response
      if status /= 200
        then throwIO (HttpError status (T.decodeUtf8Lenient (LBS.toStrict body)))
        else case J.eitherDecode body of
          Left problem -> throwIO (UnexpectedResponse (T.pack problem))
          Right value -> either (throwIO . UnexpectedResponse) pure (decodeResponse request value)

-- | The request body: the state, and each question under an id (@q0@, @q1@, …).
-- It's the core's 'A.Value' so that options keep their order.
requestBody :: Text -> JudgeRequest -> A.Value
requestBody name request =
  A.Object
    [ ("model", A.String name)
    , ("state", requestState request)
    , ("questions", A.Object [(qid, question q) | (qid, q) <- ided (requestQuestions request)])
    ]
  where
    question = \case
      AskYesNo q -> A.Object [("type", A.String "noul"), ("instructions", A.String q)]
      AskChoice q opts ->
        A.Object
          [ ("type", A.String "choice")
          , ("instructions", A.String q)
          , ("criteria", A.Object [(l, maybe A.Null A.String d) | (l, d) <- opts])
          ]
      AskScore q levels ->
        A.Object
          [ ("type", A.String "score")
          , ("instructions", A.String q)
          , ("criteria", A.Array [A.String (maybe l id d) | (l, d) <- levels])
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
