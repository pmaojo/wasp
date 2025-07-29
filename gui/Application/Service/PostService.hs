module Application.Service.PostService where

import Generated.Types
import Data.UUID (UUID)

listPosts :: IO [Post]
listPosts = pure []

findPost :: UUID -> IO (Maybe Post)
findPost _ = pure Nothing

createPost :: Post -> IO ()
createPost _ = pure ()

updatePost :: Post -> IO ()
updatePost _ = pure ()

deletePost :: UUID -> IO ()
deletePost _ = pure ()
