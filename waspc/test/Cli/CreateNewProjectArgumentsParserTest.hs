module Cli.CreateNewProjectArgumentsParserTest where

import Test.Tasty.Hspec
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs, NewProjectArgs(..))

spec_Cli_CreateNewProjectArgumentsParser :: Spec
spec_Cli_CreateNewProjectArgumentsParser =
  describe "parseNewProjectArgs" $ do
    it "parses provider option" $ do
      parseNewProjectArgs ["myapp", "--provider", "openai"]
        `shouldBe` Right (NewProjectArgs (Just "myapp") Nothing (Just "openai"))
    it "uses default provider when none specified" $ do
      parseNewProjectArgs ["myapp"]
        `shouldBe` Right (NewProjectArgs (Just "myapp") Nothing Nothing)

