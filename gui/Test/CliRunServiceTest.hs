module CliRunServiceTest where

import Test.Tasty.Hspec
import Application.Service.CliRunService
import Data.UUID (UUID)

spec_cliRunService :: Spec
spec_cliRunService = describe "CliRunService" $ do
    it "listCliRuns returns []" $ do
        runs <- listCliRuns
        runs `shouldBe` []
    it "findCliRun returns Nothing for unknown id" $ do
        let sampleId = read "00000000-0000-0000-0000-000000000000" :: UUID
        result <- findCliRun sampleId
        result `shouldBe` Nothing
