module Application.View.CliRuns.Show where

import Application.View.Prelude
import Application.Controller.CliRunsController (CliRunsController (CliRunsAction, ShowCliRunAction))
import qualified Data.Text as T

data ShowView = ShowView { cliRun :: CliRun }

instance View ShowView where
    html ShowView { .. } =
        layout
            [hsx|
                <h1>Cli Run Detail</h1>
                <p><strong>Command:</strong> {cliRunCommand cliRun}</p>
                <p><strong>Args:</strong> {T.intercalate " " (cliRunArgs cliRun)}</p>
                <p><strong>Status:</strong> {cliRunStatus cliRun}</p>
                <pre>{cliRunOutput cliRun}</pre>
                <p><a href={pathTo CliRunsAction}>Back</a></p>
            |]
