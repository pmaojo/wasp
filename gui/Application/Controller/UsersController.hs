{-# LANGUAGE OverloadedStrings #-}
module Application.Controller.UsersController where

import Application.Controller.Prelude
import Application.View.Users.Index

instance Controller UsersController where
    action UsersAction = do
        users <- liftIO listUsers
        render IndexView { .. }

    action UsersJsonAction = do
        users <- liftIO listUsers
        renderJson users

data UsersController
    = UsersAction
    | UsersJsonAction
    deriving (Eq, Show)

