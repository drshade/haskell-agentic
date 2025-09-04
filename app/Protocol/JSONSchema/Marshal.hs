module Protocol.JSONSchema.Marshal where

import           Agentic                     hiding (schemaOf)
import           Autodocodec                 (HasCodec,
                                              eitherDecodeJSONViaCodec,
                                              encodeJSONViaCodec)
import           Autodocodec.Schema          (jsonSchemaViaCodec)
import           Data.Aeson                  (encode)
import qualified Data.ByteString.Lazy        as LB (fromStrict)
import           Data.Either.Combinators     (mapLeft)
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (encodeUtf8)
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TLE
import qualified Protocol.JSONSchema.Prompts as Prompts

schemaOf :: forall s. HasCodec s => Text
schemaOf = TL.toStrict $ TLE.decodeUtf8 $ encode $ jsonSchemaViaCodec @s

parse :: forall s m. HasCodec s => Agentic m Text (Either Text s)
parse = Agentic $ \input -> do
    let result :: Either Text s
        result = mapLeft pack $ eitherDecodeJSONViaCodec (LB.fromStrict $ encodeUtf8 input)
    pure result

injectSchema :: forall s m. HasCodec s => Agentic m Prompt Prompt
injectSchema = Agentic $ \(Prompt _system user) ->
    pure $ Prompt Prompts.protocolReference (Prompts.injectSchema user (schemaOf @s))

inject :: forall s m. HasCodec s => s -> Agentic m Prompt Prompt
inject obj = Agentic $ \(Prompt system user) -> do
    let json :: Text
        json = TL.toStrict $ TLE.decodeUtf8 $ encodeJSONViaCodec obj
    pure $ Prompt system (Prompts.injectObject user json)

