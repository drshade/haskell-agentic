
module LLM where

import           OpenAI.V1
import           OpenAI.V1.Chat.Completions

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.Text                  as Text
import qualified Data.Vector                as Vector (toList)
import qualified System.Environment         as Environment

chat :: MonadIO m => Text.Text -> m Text.Text
chat input = liftIO $ do
    key <- Environment.getEnv "OPENAI_KEY"
    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = [ Text{ text = input } ], name = Nothing } ]
        , model = "gpt-5"
        }

    let result = Text.intercalate "\n" $ Vector.toList $ (\Choice{ message } -> messageToContent message) <$> choices

    pure result
