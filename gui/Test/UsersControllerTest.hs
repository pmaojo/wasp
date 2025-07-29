module UsersControllerTest where

import Test.Tasty.Hspec
import Application.Controller.UsersController

spec_usersController :: Spec
spec_usersController = describe "UsersController" $ do
    it "has UsersAction" $ do
        show UsersAction `shouldBe` "UsersAction"
