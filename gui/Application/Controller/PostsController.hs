{-# LANGUAGE OverloadedStrings #-}
module Application.Controller.PostsController where

import Application.Controller.Prelude
import Application.View.Posts.Index

instance Controller PostsController where
    action PostsAction = do
        posts <- liftIO listPosts
        render IndexView { .. }

    action PostsJsonAction = do
        posts <- liftIO listPosts
        renderJson posts

data PostsController
    = PostsAction
    | PostsJsonAction
    deriving (Eq, Show)

