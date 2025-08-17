{-# LANGUAGE OverloadedStrings #-}
module Application.Controller.AdminController where

import Application.Controller.Prelude
import Application.View.Admin.Index

instance Controller AdminController where
    action AdminAction = render IndexView

-- | Controller for admin dashboard.
data AdminController = AdminAction deriving (Eq, Show)
