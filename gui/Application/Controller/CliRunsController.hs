{-# LANGUAGE OverloadedStrings #-}
module Application.Controller.CliRunsController where

import Application.Controller.Prelude
import Application.View.CliRuns.Index
import Application.View.CliRuns.Show
import Data.UUID (UUID)

instance Controller CliRunsController where
    action CliRunsAction = do
        cliRuns <- liftIO listCliRuns
        render IndexView { .. }

    action CliRunsJsonAction = do
        cliRuns <- liftIO listCliRuns
        renderJson cliRuns

    action ShowCliRunAction { cliRunId } = do
        maybeRun <- liftIO $ findCliRun cliRunId
        case maybeRun of
            Just cliRun -> render ShowView { .. }
            Nothing -> renderPlain "Run not found"

    action ShowCliRunJsonAction { cliRunId } = do
        maybeRun <- liftIO $ findCliRun cliRunId
        case maybeRun of
            Just cliRun -> renderJson cliRun
            Nothing -> renderNotFound

data CliRunsController
    = CliRunsAction
    | CliRunsJsonAction
    | ShowCliRunAction { cliRunId :: UUID }
    | ShowCliRunJsonAction { cliRunId :: UUID }
    deriving (Eq, Show)
