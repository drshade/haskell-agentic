
module LLM.Anthropic.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types    (Parser)
import           Data.Text           (Text)
import           GHC.Generics

-- Content types for messages
data ContentBlock
  = TextContent
      { text :: Text
      }
  | ImageContent
      { source :: ImageSource
      }
  deriving (Show, Eq, Generic)

data ImageSource = ImageSource
  { sourceType :: Text
  , mediaType  :: Text
  , sourceData :: Text
  } deriving (Show, Eq, Generic)

-- Message structure
data Message = Message
  { role    :: Text
  , content :: [ContentBlock]
  } deriving (Show, Eq, Generic)

-- Tool definitions
data Tool = Tool
  { name        :: Text
  , description :: Maybe Text
  , inputSchema :: Value
  } deriving (Show, Eq, Generic)

data ToolChoice
  = ToolChoiceAuto { toolChoiceType :: Text }
  | ToolChoiceAny { toolChoiceType :: Text }
  | ToolChoiceNone { toolChoiceType :: Text }
  | ToolChoiceSpecific { toolChoiceType :: Text, name :: Text }
  deriving (Show, Eq, Generic)

-- System prompt
data SystemPrompt = SystemPrompt
  { text         :: Text
  , cacheControl :: Maybe CacheControl
  } deriving (Show, Eq, Generic)

data CacheControl = CacheControl
  { cacheControlType :: CacheType
  , ttl :: Maybe CacheTTL
  } deriving (Show, Eq, Generic)

data CacheType = Ephemeral
  deriving (Show, Eq, Generic)

data CacheTTL = FiveMinutes | OneHour
  deriving (Show, Eq, Generic)

-- Metadata
newtype Metadata = Metadata
  { userId :: Maybe Text
  } deriving (Show, Eq, Generic)

-- Request structure
data MessagesRequest = MessagesRequest
  { model         :: Text
  , messages      :: [Message]
  , maxTokens     :: Int
  , system        :: Maybe [SystemPrompt]
  , temperature   :: Maybe Double
  , tools         :: Maybe [Tool]
  , toolChoice    :: Maybe ToolChoice
  , stopSequences :: Maybe [Text]
  , stream        :: Maybe Bool
  , metadata      :: Maybe Metadata
  } deriving (Show, Eq, Generic)

-- Response structures
data MessagesResponse = MessagesResponse
  { responseId   :: Text
  , responseType :: Text
  , role         :: Text
  , content      :: [ResponseContentBlock]
  , model        :: Text
  , stopReason   :: Maybe Text
  , stopSequence :: Maybe Text
  , usage        :: Usage
  } deriving (Show, Eq, Generic)

data ResponseContentBlock
  = ResponseTextContent
      { text :: Text
      }
  | ResponseToolUseContent
      { responseContentId :: Text
      , name :: Text
      , input :: Value
      }
  deriving (Show, Eq, Generic)

data Usage = Usage
  { inputTokens  :: Int
  , outputTokens :: Int
  } deriving (Show, Eq, Generic)

-- JSON instances using Template Haskell
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', omitNothingFields = True} ''ImageSource)
-- Custom JSON instances for ContentBlock
instance ToJSON ContentBlock where
  toJSON (TextContent txt) = object
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ]
  toJSON (ImageContent src) = object
    [ "type" .= ("image" :: Text)
    , "source" .= src
    ]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \o -> do
    t <- o .: "type"
    case t :: Text of
      "text" -> TextContent <$> o .: "text"
      "image" -> ImageContent <$> o .: "source"
      _ -> fail "Unknown content type"
$(deriveJSON defaultOptions{omitNothingFields = True} ''Message)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', omitNothingFields = True} ''Tool)
$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "toolChoiceType" -> "type"
    _                -> camelTo2 '_' str
, omitNothingFields = True} ''ToolChoice)
-- Custom JSON instances for CacheType
instance ToJSON CacheType where
  toJSON Ephemeral = "ephemeral"

instance FromJSON CacheType where
  parseJSON = withText "CacheType" $ \case
    "ephemeral" -> pure Ephemeral
    _ -> fail "Invalid cache type"

-- Custom JSON instances for CacheTTL
instance ToJSON CacheTTL where
  toJSON FiveMinutes = "5m"
  toJSON OneHour = "1h"

instance FromJSON CacheTTL where
  parseJSON = withText "CacheTTL" $ \case
    "5m" -> pure FiveMinutes
    "1h" -> pure OneHour
    _ -> fail "Invalid cache TTL"

$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "cacheControlType" -> "type"
    _                  -> camelTo2 '_' str
, omitNothingFields = True} ''CacheControl)
-- Custom JSON instances for SystemPrompt
instance ToJSON SystemPrompt where
  toJSON (SystemPrompt txt cc) = object $ 
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ] ++ case cc of
      Nothing -> []
      Just c -> ["cache_control" .= c]

instance FromJSON SystemPrompt where
  parseJSON = withObject "SystemPrompt" $ \o -> do
    _ <- o .: "type" :: Parser Text  -- Expect "text" but don't use it
    txt <- o .: "text"
    cc <- o .:? "cache_control"
    pure $ SystemPrompt txt cc
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', omitNothingFields = True} ''Metadata)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', omitNothingFields = True} ''Usage)
-- Custom JSON instances for ResponseContentBlock
instance ToJSON ResponseContentBlock where
  toJSON (ResponseTextContent txt) = object
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ]
  toJSON (ResponseToolUseContent rid n inp) = object
    [ "type" .= ("tool_use" :: Text)
    , "id" .= rid
    , "name" .= n
    , "input" .= inp
    ]

instance FromJSON ResponseContentBlock where
  parseJSON = withObject "ResponseContentBlock" $ \o -> do
    t <- o .: "type"
    case t :: Text of
      "text" -> ResponseTextContent <$> o .: "text"
      "tool_use" -> ResponseToolUseContent <$> o .: "id" <*> o .: "name" <*> o .: "input"
      _ -> fail "Unknown response content type"
$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "responseId"   -> "id"
    "responseType" -> "type"
    _              -> camelTo2 '_' str
, omitNothingFields = True} ''MessagesResponse)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', omitNothingFields = True} ''MessagesRequest)
