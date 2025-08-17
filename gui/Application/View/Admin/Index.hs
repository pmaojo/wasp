module Application.View.Admin.Index where

import Application.View.Prelude

data IndexView = IndexView

instance View IndexView where
    html IndexView = layout [hsx|
        <h1>Admin Dashboard</h1>
        <ul>
            <li><a href="/Admin/RunCli">Run CLI Command</a></li>
            <li><a href="/CliRuns">CLI Runs</a></li>
            <li><a href="/Users">Users</a></li>
            <li><a href="/Posts">Posts</a></li>
        </ul>
    |]
