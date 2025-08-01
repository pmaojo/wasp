{-# LANGUAGE OverloadedStrings #-}
module Application.Controller.CliController where

import Application.Controller.Prelude
import Application.Service.CliService (runCliCommand)
import Data.Aeson (object, (.=))

instance Controller CliController where
    action CliAction = do
        cmd <- param @Text "cmd"
        jobId <- liftIO $ runCliCommand cmd
        renderJson $ object ["jobId" .= jobId]

-- | Controller for running CLI commands.
data CliController = CliAction deriving (Eq, Show)
