{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Wasp.AI.OpenAI.ChatGPT
  ( OpenAIProvider (..),
    queryChatGPT,
    ChatGPTParams (..),
    Model (..)
  )
where

import Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.UTF8 as BSU
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.HTTP.Conduit as HTTP.C
import qualified Network.HTTP.Simple as HTTP
import Wasp.AI.LLM
  ( ChatMessage (..),
    ChatResponse,
    ChatResponseUsage,
    ChatResponseChoice,
    LLMProvider (..)
  )
import Wasp.AI.OpenAI (OpenAIApiKey)
import qualified Wasp.Util as Util
import Wasp.Util.Debug (debugTrace)
import qualified Wasp.Util.Network.HTTP as Utils.HTTP

-- | OpenAI provider configuration.
newtype OpenAIProvider = OpenAIProvider {_openAIApiKey :: OpenAIApiKey}

instance LLMProvider OpenAIProvider where
  type Params OpenAIProvider = ChatGPTParams
  queryMessages (OpenAIProvider apiKey) = queryChatGPT apiKey

-- | Might throw an HttpException.
queryChatGPT :: OpenAIApiKey -> ChatGPTParams -> [ChatMessage] -> IO ChatResponse
queryChatGPT apiKey params requestMessages = do
  let reqBodyJson =
        Aeson.object $
          [ "model" .= show (_model params),
            "messages" .= requestMessages
          ]
            <> ["temperature" .= t | Just t <- pure $ _temperature params]
      request =
        -- 90 seconds should be more than enough for ChatGPT to generate an answer, or reach its own timeout.
        -- If it proves in the future that it might need more time, we can increase this number.
        HTTP.setRequestResponseTimeout (HTTP.C.responseTimeoutMicro $ Util.secondsToMicroSeconds 300) $
          HTTP.setRequestHeader "Authorization" [BSU.fromString $ "Bearer " <> apiKey] $
            HTTP.setRequestBodyJSON reqBodyJson $
              HTTP.parseRequest_ "POST https://api.openai.com/v1/chat/completions"

  (chatResponse :: ChatResponse) <-
    Utils.HTTP.httpJSONThatThrowsIfNot2xx request
      <&> either (error . ("Failed to parse ChatGPT response body as JSON: " <>)) Prelude.id

  debugTrace
    ( "\n\n\n\n==================================\n"
        <> "\n===== GPT PARAMS ======\n"
        <> show params
        <> "\n====== REQUEST =======\n"
        <> show requestMessages
        <> "\n====== RESPONSE ======\n"
        <> show chatResponse
        <> "\n==================================\n\n\n\n"
    )
    $ return ()

  return chatResponse

data ChatGPTParams = ChatGPTParams
  { _model :: !Model,
    _temperature :: !(Maybe Float)
    -- TODO: There are more parameters that chatGPT supports,
    --   we just didn't add them for now!
    --   Check https://platform.openai.com/docs/api-reference/completions/create for complete list.
  }
  deriving (Show)

data Model
  = -- New flagship model.
    GPT_4o -- Alias model
  | GPT_4o_2024_08_06
  | -- Faster & cheaper version of the new flagship model.
    GPT_4o_mini -- Alias model
  | GPT_4o_mini_2024_07_18
  | -- Old flagship model.
    GPT_4 -- Alias model
  | GPT_4_0613
  | -- Faster & cheaper version of the old flagship model.
    GPT_4_turbo -- Alias model
  | GPT_4_turbo_2024_04_09
  deriving (Eq, Bounded, Enum)

instance Show Model where
  show = modelOpenAiId

modelOpenAiId :: Model -> String
modelOpenAiId = \case
  GPT_4o -> "gpt-4o"
  GPT_4o_2024_08_06 -> "gpt-4o-2024-08-06"
  GPT_4o_mini -> "gpt-4o-mini"
  GPT_4o_mini_2024_07_18 -> "gpt-4o-mini-2024-07-18"
  GPT_4 -> "gpt-4"
  GPT_4_0613 -> "gpt-4-0613"
  GPT_4_turbo -> "gpt-4-turbo"
  GPT_4_turbo_2024_04_09 -> "gpt-4-turbo-2024-04-09"

instance FromJSON Model where
  parseJSON = Aeson.withText "Model" $ \t ->
    let t' = T.unpack t
        models = [minBound .. maxBound]
     in find ((== t') . modelOpenAiId) models
          & maybe (fail $ "Invalid GPT model: " <> t') pure
