module Application.View.CliRuns.Index where

import Application.View.Prelude

data IndexView = IndexView { cliRuns :: [CliRun] }

instance View IndexView where
    html IndexView { .. } = layout [hsx|<h1>Cli Runs</h1>|]
