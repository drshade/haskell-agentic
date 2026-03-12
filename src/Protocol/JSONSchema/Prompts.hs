module Protocol.JSONSchema.Prompts where

import Data.Text
import Text.Printf (printf)

protocolReference :: Text
protocolReference =
    pack
        """
        You reply to all responses in JSON format only, do not include any markdown or any other lead-in syntax. Just output pure JSON as a bare string.
        """

injectSchema :: Text -> Text -> Text
injectSchema prompt schema =
    pack $ printf
      """
      %s

      Return response in JSON format conforming to the following schema:
      %s
      """ prompt schema

injectObject :: Text -> Text -> Text
injectObject prompt object =
    pack $ printf
      """
      %s

      Input:
      %s
      """ prompt object