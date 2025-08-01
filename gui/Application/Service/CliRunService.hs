module Application.Service.CliRunService where

import Generated.Types
import Data.UUID (UUID)

listCliRuns :: IO [CliRun]
listCliRuns = pure []

findCliRun :: UUID -> IO (Maybe CliRun)
findCliRun _ = pure Nothing

createCliRun :: CliRun -> IO ()
createCliRun _ = pure ()

updateCliRun :: CliRun -> IO ()
updateCliRun _ = pure ()

deleteCliRun :: UUID -> IO ()
deleteCliRun _ = pure ()
