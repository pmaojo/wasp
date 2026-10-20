module Generated.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data User = User
    { userId :: UUID
    , userEmail :: Text
    , userName :: Text
    , userCreatedAt :: UTCTime
    }
    deriving (Eq, Show)

data Post = Post
    { postId :: UUID
    , postUserId :: UUID
    , postTitle :: Text
    , postBody :: Text
    , postCreatedAt :: UTCTime
    }
    deriving (Eq, Show)

data CliRun = CliRun
    { cliRunId :: UUID
    , cliRunCommand :: Text
    , cliRunArgs :: [Text]
    , cliRunStatus :: Text
    , cliRunOutput :: Text
    , cliRunCreatedAt :: UTCTime
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
