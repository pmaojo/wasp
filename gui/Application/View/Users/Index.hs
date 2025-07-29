module Application.View.Users.Index where

import Application.View.Prelude

data IndexView = IndexView { users :: [User] }

instance View IndexView where
    html IndexView { .. } = layout [hsx|<h1>Users</h1>|]
