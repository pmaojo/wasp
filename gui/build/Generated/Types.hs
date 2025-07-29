module Generated.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

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
