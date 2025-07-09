{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Types used by Wasp Studio for representing the structure of a Wasp app.
-- These mirror the JSON consumed by the Studio client.
module Wasp.Studio.Data
  ( StudioData (..)
  , StudioPage (..)
  , PageRef (..)
  , StudioRoute (..)
  , HttpRoute (..)
  , StudioApi (..)
  , StudioJob (..)
  , OperationType (..)
  , StudioOperation (..)
  , StudioCrud (..)
  , StudioEntity (..)
  , StudioAppAuth (..)
  , StudioDb (..)
  , StudioApp (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

-- | Top level data consumed by Studio.
data StudioData = StudioData
  { pages :: [StudioPage]
  , routes :: [StudioRoute]
  , apis :: [StudioApi]
  , jobs :: [StudioJob]
  , operations :: [StudioOperation]
  , cruds :: [StudioCrud]
  , entities :: [StudioEntity]
  , app :: StudioApp
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Represents a page in the app.
data StudioPage = StudioPage
  { name :: String
  , authRequired :: Maybe Bool
  , operations :: [String]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Reference to a page by name.
newtype PageRef = PageRef { name :: String }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Application route.
data StudioRoute = StudioRoute
  { name :: String
  , path :: String
  , toPage :: PageRef
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | HTTP route of an API.
data HttpRoute = HttpRoute
  { method :: String
  , path :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | API declaration.
data StudioApi = StudioApi
  { name :: String
  , httpRoute :: HttpRoute
  , auth :: Maybe Bool
  , entities :: [StudioEntity]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Job declaration.
data StudioJob = StudioJob
  { name :: String
  , schedule :: Maybe String
  , entities :: [StudioEntity]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Supported operation types.
data OperationType = Query | Action
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Query or action.
data StudioOperation = StudioOperation
  { type_ :: OperationType
  , name :: String
  , entities :: [StudioEntity]
  , auth :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON StudioOperation where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

instance FromJSON StudioOperation where
  parseJSON = Aeson.genericParseJSON options

options :: Aeson.Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \case
        "type_" -> "type"
        other -> other
    }

-- | CRUD declaration.
data StudioCrud = StudioCrud
  { name :: String
  , operations :: [String]
  , entities :: [StudioEntity]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Entity declaration.
newtype StudioEntity = StudioEntity { name :: String }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Authentication info of the app.
data StudioAppAuth = StudioAppAuth
  { userEntity :: StudioEntity
  , methods :: [String]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Database info of the app.
newtype StudioDb = StudioDb { system :: String }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Top-level app information.
data StudioApp = StudioApp
  { name :: String
  , auth :: StudioAppAuth
  , db :: StudioDb
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

