module RootApplication
    ( RootApplication(..)
    ) where

-- | Marker type for the IHP application.
--   Exported so other modules can reference it.
data RootApplication = RootApplication
    deriving (Eq, Show)
