module Application.View.Admin.RunCli where

import Application.View.Prelude
import Application.Controller.AdminController (AdminController(..))
import Data.UUID (UUID)

-- | View for running CLI commands from the admin interface.
data RunCliView = RunCliView { jobId :: Maybe UUID }

instance View RunCliView where
    html RunCliView { .. } = layout [hsx|
        <h1>Run CLI Command</h1>
        {formFor RunCliAction [hsx|
            <div class="form-group">
                <input type="text" name="cmd" class="form-control" placeholder="Enter command" />
            </div>
            <button type="submit" class="btn btn-primary">Run</button>
        |]}
        {case jobId of
            Just jid -> [hsx|<p>Command started with Job ID: {show jid}</p>|]
            Nothing -> mempty
        }
    |]
