module UserServiceTest where

import Test.Tasty.Hspec
import Application.Service.UserService

spec_userService :: Spec
spec_userService = describe "UserService" $ do
    it "listUsers returns []" $ do
        users <- listUsers
        users `shouldBe` []
