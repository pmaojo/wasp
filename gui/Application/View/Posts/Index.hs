module Application.View.Posts.Index where

import Application.View.Prelude

data IndexView = IndexView { posts :: [Post] }

instance View IndexView where
    html IndexView { .. } = layout [hsx|<h1>Posts</h1>|]
