{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic low level interface for Large Language Models.
--   Contains provider neutral data types and a typeclass
--   describing minimal operations used by Wasp.
module Wasp.AI.LLM
  ( ChatMessage (..)
  , ChatRole (..)
  , ChatResponse (..)
  , ChatResponseUsage (..)
  , ChatResponseChoice (..)
  , getChatResponseContent
  , LLMProvider (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Single message exchanged with LLM.
data ChatMessage = ChatMessage
  { role :: !ChatRole
  , content :: !Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Role of the chat participant.
data ChatRole = User | System | Assistant
  deriving (Generic, Show)

instance ToJSON ChatRole where
  toJSON User = Aeson.String "user"
  toJSON System = Aeson.String "system"
  toJSON Assistant = Aeson.String "assistant"

instance FromJSON ChatRole where
  parseJSON = Aeson.withText "ChatRole" $ \case
    "user" -> pure User
    "system" -> pure System
    "assistant" -> pure Assistant
    other -> fail $ "Invalid ChatRole: " <> show other

-- | Response returned by an LLM.
data ChatResponse = ChatResponse
  { id :: !String
  , object :: !String
  , created :: !Int
  , model :: !String
  , choices :: ![ChatResponseChoice]
  , usage :: !ChatResponseUsage
  }
  deriving (Generic, Show, FromJSON)

-- | Usage statistics returned by the provider.
data ChatResponseUsage = ChatResponseUsage
  { prompt_tokens :: !Int
  , completion_tokens :: !Int
  , total_tokens :: !Int
  }
  deriving (Generic, Show, FromJSON)

-- | Single choice within a response.
data ChatResponseChoice = ChatResponseChoice
  { index :: !Int
  , message :: !ChatMessage
  , finish_reason :: !String
  }
  deriving (Generic, Show, FromJSON)

-- | Extracts assistant content from a response.
getChatResponseContent :: ChatResponse -> Text
getChatResponseContent = content . message . head . choices

-- | Abstraction over concrete LLM providers.
class LLMProvider p where
  -- | Parameters used when querying the provider.
  type Params p
  -- | Perform a query with given parameters and messages.
  queryMessages :: p -> Params p -> [ChatMessage] -> IO ChatResponse
