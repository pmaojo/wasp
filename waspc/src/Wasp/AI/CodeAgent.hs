{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.AI.CodeAgent
  ( CodeAgent,
    CodeAgentConfig (..),
    runCodeAgent,
    writeToLog,
    writeToFile,
    writeNewFile,
    getFile,
    getAllFiles,
    queryLLM,
    getTotalTokensUsage
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT (runStateT), gets, modify)
import qualified Data.HashMap.Strict as H
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Network.HTTP.Simple as HTTP
import System.IO (hPutStrLn, stderr)
import UnliftIO (Handler (Handler), catches, throwIO)
import Wasp.AI.LLM
  ( ChatMessage,
    ChatResponse,
    ChatResponseUsage,
    LLMProvider (..),
    getChatResponseContent,
  )
import Wasp.AI.OpenAI.ChatGPT (OpenAIProvider, ChatGPTParams)
import qualified Wasp.Util as Util
import Wasp.Util.IO.Retry (MonadRetry)
import qualified Wasp.Util.IO.Retry as R
import Wasp.Util.Network.HTTP (catchRetryableHttpException)
import qualified Wasp.Util.Network.HTTP as Utils.HTTP

newtype GenericCodeAgent provider logMsg a =
  GenericCodeAgent {_unCodeAgent :: ReaderT (CodeAgentConfig provider logMsg) (StateT CodeAgentState IO) a}
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader (CodeAgentConfig provider logMsg), MonadState CodeAgentState)

type CodeAgent = GenericCodeAgent OpenAIProvider

data CodeAgentConfig provider logMsg = CodeAgentConfig
  { _llmProvider :: !provider,
    _writeFile :: !(FilePath -> Text -> IO ()),
    _writeLog :: !(logMsg -> IO ())
  }

instance MonadRetry (GenericCodeAgent provider logMsg) where
  rThreadDelay = liftIO . threadDelay

runCodeAgent :: (IsString logMsg) => CodeAgentConfig provider logMsg -> GenericCodeAgent provider logMsg a -> IO a
runCodeAgent config codeAgent =
  (fst <$> (_unCodeAgent codeAgent `runReaderT` config) `runStateT` initialState)
    `catches` [ Handler
                  ( \(e :: HTTP.HttpException) -> do
                      let errorInfo =
                            maybe (showShortException e) show $ Utils.HTTP.getHttpExceptionStatusCode e
                          logMsg = fromString $ "Code agent failed with the http error: " <> errorInfo
                      _writeLog config logMsg
                      throwIO e
                  ),
                Handler
                  ( \(e :: SomeException) -> do
                      _writeLog config $
                        fromString $ "Code agent failed with the following error: " <> showShortException e
                      throwIO e
                  )
              ]
  where
    initialState =
      CodeAgentState
        { _files = H.empty,
          _usage = []
        }

    shortenWithEllipsisTo maxLen text =
      if length text <= maxLen
        then text
        else take maxLen text <> "..."

    showShortException :: forall e. Exception e => e -> String
    showShortException = shortenWithEllipsisTo 30 . displayException

writeToLog :: IsString logMsg => logMsg -> GenericCodeAgent provider logMsg ()
writeToLog msg = asks _writeLog >>= \f -> liftIO $ f msg

writeToFile :: FilePath -> (Maybe Text -> Text) -> GenericCodeAgent provider logMsg ()
writeToFile path updateContentFn = do
  content <- updateContentFn <$> getFile path
  asks _writeFile >>= \f -> liftIO $ f path content
  modify $ \s -> s {_files = H.insert path content (_files s)}

writeNewFile :: (FilePath, Text) -> GenericCodeAgent provider logMsg ()
writeNewFile (path, content) =
  writeToFile path (maybe content $ error $ "file " <> path <> " shouldn't already exist")

getFile :: FilePath -> GenericCodeAgent provider logMsg (Maybe Text)
getFile path = gets $ H.lookup path . _files

getAllFiles :: GenericCodeAgent provider logMsg [(FilePath, Text)]
getAllFiles = gets $ H.toList . _files

queryLLM :: (LLMProvider provider) => Params provider -> [ChatMessage] -> GenericCodeAgent provider logMsg Text
queryLLM params messages = do
  provider <- asks _llmProvider
  chatResponse <- queryLLMWithRetry provider params messages
  modify $ \s -> s {_usage = _usage s <> [usage chatResponse]}
  return $ getChatResponseContent chatResponse
  where
{- ORMOLU_DISABLE -}
    queryLLMWithRetry :: (LLMProvider provider) => provider -> Params provider -> [ChatMessage] -> GenericCodeAgent provider logMsg ChatResponse
    queryLLMWithRetry provider' params' messages' =
      do
        R.retry
          (R.expPause $ fromIntegral $ Util.secondsToMicroSeconds 10)
          3
          ( liftIO $
              (Right <$> queryMessages provider' params' messages')
                `catchRetryableHttpException` ( \e -> do
                    hPutStrLn stderr $ "Caught retryable HTTP exception while doing ChatGPT request: "
                        <> maybe "" (\code -> "Status code: " <> show code <> "; ") (Utils.HTTP.getHttpExceptionStatusCode e)
                        <> show e
                    return $ Left e
                                              )
          )
          >>= either throwIO pure
{- ORMOLU_ENABLE -}

type NumTokens = Int

-- | Returns total tokens usage: (<num_prompt_tokens>, <num_completion_tokens>).
getTotalTokensUsage :: GenericCodeAgent provider logMsg (NumTokens, NumTokens)
getTotalTokensUsage = do
  usage <- gets _usage
  let numPromptTokens = sum $ prompt_tokens <$> usage
  let numCompletionTokens = sum $ completion_tokens <$> usage
  return (numPromptTokens, numCompletionTokens)

data CodeAgentState = CodeAgentState
  { _files :: !(H.HashMap FilePath Text), -- TODO: Name this "cacheFiles" maybe?
    _usage :: ![ChatResponseUsage]
  }
