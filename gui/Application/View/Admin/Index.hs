module Application.View.Admin.Index where

import Application.View.Prelude

data IndexView = IndexView

instance View IndexView where
    html IndexView = layout [hsx|
        <h1>Admin Dashboard</h1>
        <ul>
            <li><a href="/Cli">CLI</a></li>
            <li><a href="/Users">Users</a></li>
            <li><a href="/Posts">Posts</a></li>
        </ul>
    |]
