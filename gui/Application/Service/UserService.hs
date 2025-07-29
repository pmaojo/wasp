module Application.Service.UserService where

import Generated.Types
import Data.UUID (UUID)

listUsers :: IO [User]
listUsers = pure []

findUser :: UUID -> IO (Maybe User)
findUser _ = pure Nothing

createUser :: User -> IO ()
createUser _ = pure ()

updateUser :: User -> IO ()
updateUser _ = pure ()

deleteUser :: UUID -> IO ()
deleteUser _ = pure ()
