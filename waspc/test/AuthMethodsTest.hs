module AuthMethodsTest where

import Test.Tasty.Hspec
import Wasp.AppSpec.App.Auth
  ( AuthMethods(..),
    UsernameAndPasswordConfig,
    ExternalAuthConfig,
    EmailAuthConfig
  )
import Wasp.AppSpec.App.Auth.Util (enabledAuthMethodNames)

spec_AuthMethodsTest :: Spec
spec_AuthMethodsTest = describe "enabledAuthMethodNames" $ do
  it "returns empty list when no methods enabled" $ do
    let methods = AuthMethods Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    enabledAuthMethodNames methods `shouldBe` []

  it "detects a single enabled method" $ do
    let methods = AuthMethods (Just undefined) Nothing Nothing Nothing Nothing Nothing Nothing
    enabledAuthMethodNames methods `shouldBe` ["usernameAndPassword"]

  it "handles multiple enabled methods" $ do
    let methods = AuthMethods
          { usernameAndPassword = Just undefined
          , slack = Just undefined
          , discord = Nothing
          , google = Just undefined
          , gitHub = Nothing
          , keycloak = Nothing
          , email = Nothing
          }
    enabledAuthMethodNames methods `shouldBe` ["usernameAndPassword", "slack", "google"]

