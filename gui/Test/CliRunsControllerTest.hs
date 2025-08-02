module CliRunsControllerTest where

import Test.Tasty.Hspec
import Application.Controller.CliRunsController
import Data.UUID (UUID)

spec_cliRunsController :: Spec
spec_cliRunsController = describe "CliRunsController" $ do
    it "has CliRunsAction" $ do
        show CliRunsAction `shouldBe` "CliRunsAction"
    it "has ShowCliRunAction" $ do
        let sampleId = read "00000000-0000-0000-0000-000000000000" :: UUID
        case ShowCliRunAction sampleId of
            ShowCliRunAction{} -> True `shouldBe` True
