{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LLM.Anthropic.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

-- Content types for messages
data ContentBlock 
  = TextContent 
      { contentType :: Text
      , text :: Text
      }
  | ImageContent
      { contentType :: Text
      , source :: ImageSource
      }
  deriving (Show, Eq, Generic)

data ImageSource = ImageSource
  { sourceType :: Text
  , mediaType :: Text
  , sourceData :: Text
  } deriving (Show, Eq, Generic)

-- Message structure
data Message = Message
  { role :: Text
  , content :: Either Text [ContentBlock]
  } deriving (Show, Eq, Generic)

-- Tool definitions
data Tool = Tool
  { name :: Text
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
  { text :: Text
  , cacheControl :: Maybe CacheControl
  } deriving (Show, Eq, Generic)

newtype CacheControl = CacheControl
  { cacheControlType :: Text
  } deriving (Show, Eq, Generic)

-- Metadata
newtype Metadata = Metadata
  { userId :: Maybe Text
  } deriving (Show, Eq, Generic)

-- Request structure
data MessagesRequest = MessagesRequest
  { model :: Text
  , messages :: [Message]
  , maxTokens :: Int
  , system :: Maybe (Either Text [SystemPrompt])
  , temperature :: Maybe Double
  , tools :: Maybe [Tool]
  , toolChoice :: Maybe ToolChoice
  , stopSequences :: Maybe [Text]
  , stream :: Maybe Bool
  , metadata :: Maybe Metadata
  } deriving (Show, Eq, Generic)

-- Response structures
data MessagesResponse = MessagesResponse
  { responseId :: Text
  , responseType :: Text
  , role :: Text
  , content :: [ResponseContentBlock]
  , model :: Text
  , stopReason :: Maybe Text
  , stopSequence :: Maybe Text
  , usage :: Usage
  } deriving (Show, Eq, Generic)

data ResponseContentBlock
  = ResponseTextContent
      { responseContentType :: Text
      , text :: Text
      }
  | ResponseToolUseContent
      { responseContentType :: Text
      , responseContentId :: Text
      , name :: Text
      , input :: Value
      }
  deriving (Show, Eq, Generic)

data Usage = Usage
  { inputTokens :: Int
  , outputTokens :: Int
  } deriving (Show, Eq, Generic)

-- JSON instances using Template Haskell
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''ImageSource)
$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "contentType" -> "type"
    _ -> camelTo2 '_' str
} ''ContentBlock)
$(deriveJSON defaultOptions ''Message)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Tool)
$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "toolChoiceType" -> "type"
    _ -> camelTo2 '_' str
} ''ToolChoice)
$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "cacheControlType" -> "type"
    _ -> camelTo2 '_' str
} ''CacheControl)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''SystemPrompt)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Metadata)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Usage)
$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "responseContentType" -> "type"
    "responseContentId" -> "id"
    _ -> camelTo2 '_' str
} ''ResponseContentBlock)
$(deriveJSON defaultOptions{fieldLabelModifier = \str -> case str of
    "responseId" -> "id"
    "responseType" -> "type"
    _ -> camelTo2 '_' str
} ''MessagesResponse)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''MessagesRequest)