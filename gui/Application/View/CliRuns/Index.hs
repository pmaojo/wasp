module Application.View.CliRuns.Index where

import Application.View.Prelude
import Application.Controller.CliRunsController (CliRunsController (ShowCliRunAction))

data IndexView = IndexView { cliRuns :: [CliRun] }

instance View IndexView where
    html IndexView { .. } =
        layout
            [hsx|
                <h1>Cli Runs</h1>
                <ul>
                    {forEach cliRuns renderRun}
                </ul>
            |]
      where
        renderRun run =
            [hsx|
                <li>
                    <a href={pathTo ShowCliRunAction { cliRunId = cliRunId run }}>
                        {cliRunCommand run}
                    </a>
                </li>
            |]
