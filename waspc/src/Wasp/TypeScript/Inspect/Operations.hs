{-# LANGUAGE DeriveGeneric #-}
module Wasp.TypeScript.Inspect.Operations
  ( getOperationsOfTsFiles,
    TsOperationsRequest (..),
    TsOperationsResponse (..),
  ) where

import Data.Aeson (FromJSON, ToJSON (toEncoding), decode, encode, defaultOptions, genericToEncoding)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Conduit.Process.Typed (ExitCode (ExitSuccess))
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import qualified System.Process as P
import Wasp.NodePackageFFI (RunnablePackage (TsInspectPackage), getPackageProcessOptions)

-- | Map from file path to list of operation names found in that file.
newtype TsOperationsResponse = TsOperationsResponse (M.HashMap FilePath [String])
  deriving (Eq, Show, FromJSON)

-- | Request for operations used in files.
data TsOperationsRequest = TsOperationsRequest
  { filepaths :: ![FilePath],
    tsconfig :: !(Maybe FilePath)
  }
  deriving (Eq, Show, Generic)

instance ToJSON TsOperationsRequest where
  toEncoding = genericToEncoding defaultOptions

-- | Attempt to get list of operation names referenced via useQuery/useAction hooks.
getOperationsOfTsFiles :: [TsOperationsRequest] -> IO (Either String TsOperationsResponse)
getOperationsOfTsFiles requests = do
  let requestJSON = BS.toString $ encode requests
  cp <- getPackageProcessOptions TsInspectPackage ["operations"]
  (exitCode, response, err) <- P.readCreateProcessWithExitCode cp requestJSON
  case exitCode of
    ExitSuccess -> case decode $ BS.fromString response of
      Nothing -> return $ Left $ "invalid response JSON from ts-inspect: " ++ response
      Just ops -> return $ Right ops
    _ -> return $ Left err

