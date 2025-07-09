{-# LANGUAGE TemplateHaskell #-}
module Wasp.AppSpec.App.Auth.Util
  ( enabledAuthMethodNames
  ) where

import Data.Maybe (isJust)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Wasp.AppSpec.App.Auth (AuthMethods(..))

-- | Names of fields in 'AuthMethods' that this module expects.
--   If a field is added or removed, update this list accordingly.
expectedAuthMethodFields :: [String]
expectedAuthMethodFields =
  [ "usernameAndPassword"
  , "slack"
  , "discord"
  , "google"
  , "gitHub"
  , "keycloak"
  , "email"
  ]

-- | Returns names of enabled authentication methods based on provided
--   'AuthMethods'. Compilation fails if 'AuthMethods' record fields do not
--   match 'expectedAuthMethodFields'.
enabledAuthMethodNames :: AuthMethods -> [String]
enabledAuthMethodNames = $(genEnabledAuthMethodNames)

-- | Template Haskell generator for 'enabledAuthMethodNames'. It verifies that
-- the fields of 'AuthMethods' match 'expectedAuthMethodFields' and then
-- produces a function implementation that inspects each field.
genEnabledAuthMethodNames :: Q Exp
genEnabledAuthMethodNames = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify ''AuthMethods
  let actualFields = map (nameBase . \(n,_,_) -> n) fields
  if actualFields /= expectedAuthMethodFields
    then fail $ unlines
      [ "enabledAuthMethodNames: AuthMethods fields changed.",
        "  Expected: " ++ show expectedAuthMethodFields,
        "  Actual:   " ++ show actualFields,
        "  Please update expectedAuthMethodFields and tests."
      ]
    else do
      vars <- mapM (newName . ("f" ++)) (map show [1..length actualFields])
      let pat = RecP 'AuthMethods $ zipWith (\field var -> (mkName field, VarP var)) actualFields vars
      let methodExpr (var, name) = [| if isJust $(varE var) then [$(litE (stringL name))] else [] |]
      body <- foldr1 (\a b -> [| $a ++ $b |]) <$> mapM (pure . methodExpr) (zip vars actualFields)
      lamE [pat] body

