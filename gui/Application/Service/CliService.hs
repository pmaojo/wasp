module Application.Service.CliService where

import IHP.Prelude
import qualified System.Process as Process
import Control.Concurrent.Async (async)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

-- | Runs a CLI command asynchronously. Returns a job identifier.
runCliCommand :: Text -> IO UUID
runCliCommand cmd = do
    jobId <- UUID.nextRandom
    -- Run the command asynchronously to avoid blocking the controller.
    _ <- async (Process.callCommand (cs cmd))
    pure jobId
