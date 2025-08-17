{-# LANGUAGE OverloadedStrings #-}
module Application.Controller.AdminController where

import Application.Controller.Prelude
import Application.View.Admin.Index
import Application.View.Admin.RunCli
import Application.Service.CliService (runCliCommand)

instance Controller AdminController where
    action AdminAction = render IndexView

    action RunCliAction = do
        cmd <- paramOrNothing @Text "cmd"
        case cmd of
            Just command -> do
                jobId <- liftIO $ runCliCommand command
                render RunCliView { jobId = Just jobId }
            Nothing -> render RunCliView { jobId = Nothing }

-- | Controller for admin dashboard.
data AdminController
    = AdminAction
    | RunCliAction
    deriving (Eq, Show)
