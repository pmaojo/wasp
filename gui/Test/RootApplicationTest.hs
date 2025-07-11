module RootApplicationTest where

import Test.Tasty.Hspec
import RootApplication

spec_rootApplication :: Spec
spec_rootApplication = describe "RootApplication" $ do
    it "derives Eq" $ RootApplication `shouldBe` RootApplication
    it "derives Show" $ show RootApplication `shouldBe` "RootApplication"
