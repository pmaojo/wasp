module Wasp.AI.Provider
  ( Provider(..)
  , ApiKey
  , providerEnvVar
  , getProviderApiKey
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import System.Environment (lookupEnv)

import Wasp.Cli.Command (Command, CommandError (..))

-- | Supported AI providers.
data Provider = OpenAI deriving (Show, Eq)

-- | API key used for querying the provider.
type ApiKey = String

-- | Returns the name of the env variable expected to hold the API key.
providerEnvVar :: Provider -> String
providerEnvVar OpenAI = "OPENAI_API_KEY"

-- | Retrieves API key for given provider from environment.
getProviderApiKey :: Provider -> Command ApiKey
getProviderApiKey provider =
  liftIO (lookupEnv envVar <&> (>>= validateKey)) >>= maybe throwMissingEnvVar pure
  where
    envVar = providerEnvVar provider

    validateKey "" = Nothing
    validateKey k = Just k

    throwMissingEnvVar = throwError $ CommandError ("Missing " ++ envVar ++ " environment variable") message

    message = unlines $ case provider of
      OpenAI ->
        [ "Wasp AI uses ChatGPT to generate your project and therefore requires an OpenAI API key.",
          "You can obtain this key via your OpenAI account settings: https://platform.openai.com/account/api-keys.",
          "Then, set " ++ envVar ++ " env var to it and Wasp CLI will read from it.",
          "",
          "To persist the " ++ envVar ++ " env var, add",
          "  export " ++ envVar ++ "=<yourkeyhere>",
          "to your shell profile and restart the terminal.",
          "",
          "Alternatively, you can go to our Mage web app at https://usemage.ai and generate a new Wasp app there for free."
        ]

