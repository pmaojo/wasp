module Application.Service.CliRunService where

import Data.Aeson (eitherDecodeFileStrict')
import Data.List (find)
import Data.UUID (UUID)
import Generated.Types
import System.Directory (doesFileExist)

cliRunsFile :: FilePath
cliRunsFile = "../frontend/data/cli-runs.json"

listCliRuns :: IO [CliRun]
listCliRuns = do
    exists <- doesFileExist cliRunsFile
    if exists
        then do
            result <- eitherDecodeFileStrict' cliRunsFile
            pure $ either (const []) id result
        else pure []

findCliRun :: UUID -> IO (Maybe CliRun)
findCliRun runId = do
    runs <- listCliRuns
    pure $ find ((== runId) . cliRunId) runs

createCliRun :: CliRun -> IO ()
createCliRun _ = pure ()

updateCliRun :: CliRun -> IO ()
updateCliRun _ = pure ()

deleteCliRun :: UUID -> IO ()
deleteCliRun _ = pure ()
