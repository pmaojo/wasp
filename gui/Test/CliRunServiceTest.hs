module CliRunServiceTest where

import Test.Tasty.Hspec
import Application.Service.CliRunService

spec_cliRunService :: Spec
spec_cliRunService = describe "CliRunService" $ do
    it "listCliRuns returns []" $ do
        runs <- listCliRuns
        runs `shouldBe` []
