module RootApplication
    ( RootApplication(..)
    ) where

import Application.Controller.CliController ()
-- | Marker type for the IHP application.
--   Exported so other modules can reference it.
data RootApplication = RootApplication
    deriving (Eq, Show)
