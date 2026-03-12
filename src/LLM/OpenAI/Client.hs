
module LLM.OpenAI.Client where

{-
chat :: MonadIO m => Text.Text -> Text.Text -> m Text.Text
chat system user = liftIO $ do
    key <- Environment.getEnv "OPENAI_KEY"
    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages =
            [ System{ content = [ Text{ text = system } ], name = Nothing }
            , User{ content = [ Text{ text = user } ], name = Nothing }
            ]
        -- , model = "gpt-5-mini"
        -- , model = "gpt-4o"
        , model = "o1"
        }

    let result = Text.intercalate "\n" $ Vector.toList $ (\Choice{ message } -> messageToContent message) <$> choices

    pure result
-}

