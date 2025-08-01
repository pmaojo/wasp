{-# LANGUAGE OverloadedStrings #-}
module Application.Controller.CliRunsController where

import Application.Controller.Prelude
import Application.View.CliRuns.Index

instance Controller CliRunsController where
    action CliRunsAction = do
        cliRuns <- liftIO listCliRuns
        render IndexView { .. }

    action CliRunsJsonAction = do
        cliRuns <- liftIO listCliRuns
        renderJson cliRuns

data CliRunsController
    = CliRunsAction
    | CliRunsJsonAction
    deriving (Eq, Show)
