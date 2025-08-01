module Main where
import IHP.Prelude

import Config
import RootApplication
import qualified IHP.Server
import IHP.RouterSupport
import IHP.FrameworkConfig
import IHP.Job.Types
import Application.Controller.UsersController
import Application.Controller.PostsController
import Application.Controller.CliRunsController

instance FrontController RootApplication where
    controllers =
          [ startPage UsersAction
          , parseRoute @"/Posts" PostsAction
          , parseRoute @"/Users" UsersAction
          , parseRoute @"/CliRuns" CliRunsAction
          , parseRoute @"/Users.json" UsersJsonAction
          , parseRoute @"/Posts.json" PostsJsonAction
          , parseRoute @"/CliRuns.json" CliRunsJsonAction
          ]

instance Worker RootApplication where
    workers _ = []

main :: IO ()
main = IHP.Server.run config
