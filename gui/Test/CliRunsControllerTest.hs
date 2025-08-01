module CliRunsControllerTest where

import Test.Tasty.Hspec
import Application.Controller.CliRunsController

spec_cliRunsController :: Spec
spec_cliRunsController = describe "CliRunsController" $ do
    it "has CliRunsAction" $ do
        show CliRunsAction `shouldBe` "CliRunsAction"
