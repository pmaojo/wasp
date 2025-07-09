module Wasp.Cli.Command.Studio
  ( studio,
    makeAppInfoJson,
    generateStudioDataFile,
  )
where

import Control.Arrow ()
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Operations ()
import qualified System.Directory as Dir
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import Wasp.AppSpec.App.Auth.Util (enabledAuthMethodNames)
import qualified Wasp.AppSpec.Job as AS.Job
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as Operation
import Wasp.Generator.Crud (crudDeclarationToOperationsList)
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import qualified Wasp.Project.Studio
import qualified Wasp.Studio.PageOperations as StudioOps
import qualified Data.HashMap.Strict as M
import Wasp.Studio.Data

studio :: Command ()
studio = do
  InWaspProject waspDir <- require

  appSpec <- analyze waspDir
  appInfoJson <- liftIO $ makeAppInfoJson waspDir appSpec

  let generatedProjectDir =
        waspDir </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir

  let waspStudioDataJsonFilePath = generatedProjectDir </> [relfile|.wasp-studio-data.json|]
  liftIO $ do
    Dir.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent waspStudioDataJsonFilePath
    BSL.writeFile (SP.fromAbsFile waspStudioDataJsonFilePath) (encodePretty appInfoJson)

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting Wasp Studio ✨",
        "",
        "➜ Open in your browser: http://localhost:4000",
        "",
        "Wasp Studio visualises your app and lets you understand how different parts of your app are connected."
      ]

  result <- liftIO $ do
    Wasp.Project.Studio.startStudio $ SP.toFilePath waspStudioDataJsonFilePath

  either (throwError . CommandError "Studio command failed") return result


makeAppInfoJson :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> AS.AppSpec -> IO Data.Aeson.Value
makeAppInfoJson waspDir spec = toJSON <$> makeAppInfo waspDir spec

makeAppInfo :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> AS.AppSpec -> IO StudioData
makeAppInfo waspDir spec = do
  let (appName, app) = ASV.getApp spec
  opsRes <- StudioOps.collectPageOperations waspDir spec
  let pageOps = either (const M.empty) id opsRes
  let pages =
        [ StudioPage
            { name = name
            , authRequired = AS.Page.authRequired page
            , operations = M.lookupDefault [] name pageOps
            }
        | (name, page) <- AS.getPages spec
        ]
  let routes =
        [ StudioRoute
            { name = name
            , path = AS.Route.path route
            , toPage = PageRef {name = fst (AS.resolveRef spec $ AS.Route.to route)}
            }
        | (name, route) <- AS.getRoutes spec
        ]
  let apis =
        [ StudioApi
            { name = name
            , httpRoute =
                let (method, path) = AS.Api.httpRoute api
                 in HttpRoute {method = show method, path = path}
            , auth = AS.Api.auth api
            , entities = getLinkedEntities spec (AS.Api.entities api)
            }
        | (name, api) <- AS.getApis spec
        ]
  let jobs =
        [ StudioJob
            { name = name
            , schedule = AS.Job.cron <$> AS.Job.schedule job
            , entities = getLinkedEntities spec (AS.Job.entities job)
            }
        | (name, job) <- AS.getJobs spec
        ]
  let operations =
        [ StudioOperation
            { type_ = case operation of
                QueryOp _ _ -> Query
                ActionOp _ _ -> Action
            , name = Operation.getName operation
            , entities = getLinkedEntities spec (Operation.getEntities operation)
            , auth = Operation.getAuth operation
            }
        | operation <- AS.getOperations spec
        ]
  let cruds =
        [ StudioCrud
            { name = name
            , operations = map (show . fst) (crudDeclarationToOperationsList crud)
            , entities = getLinkedEntities spec (Just [AS.Crud.entity crud])
            }
        | (name, crud) <- AS.getCruds spec
        ]
  let entities =
        [ StudioEntity {name = name}
        | (name, _) <- AS.getEntities spec
        ]
  authInfo <- getAuthInfoData spec app
  let appInfo = StudioApp {name = appName, auth = authInfo, db = getDbInfoData spec}
  return
    StudioData
      { pages = pages
      , routes = routes
      , apis = apis
      , jobs = jobs
      , operations = operations
      , cruds = cruds
      , entities = entities
      , app = appInfo
      }

-- | Generate the studio data JSON file and return its path.
generateStudioDataFile :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> AS.AppSpec -> IO (SP.Path' SP.Abs SP.File')
generateStudioDataFile waspDir spec = do
  appJson <- makeAppInfoJson waspDir spec
  let generatedProjectDir =
        waspDir SP.</> dotWaspDirInWaspProjectDir SP.</> generatedCodeDirInDotWaspDir
  let filePath = generatedProjectDir SP.</> [relfile|.wasp-studio-data.json|]
  Dir.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent filePath
  BSL.writeFile (SP.fromAbsFile filePath) (encodePretty appJson)
  return filePath

getLinkedEntities :: AS.AppSpec -> Maybe [AS.Ref AS.Entity] -> [StudioEntity]
getLinkedEntities spec entityRefs =
  map (\(entityName, _entity) -> StudioEntity {name = entityName}) $
    resolveEntities spec entityRefs

resolveEntities :: AS.AppSpec -> Maybe [AS.Ref AS.Entity] -> [(String, AS.Entity)]
resolveEntities spec entityRefs = AS.resolveRef spec <$> fromMaybe [] entityRefs

getDbInfoData :: AS.AppSpec -> StudioDb
getDbInfoData spec = StudioDb {system = show (ASV.getValidDbSystem spec)}

getAuthInfoData :: AS.AppSpec -> AS.App.App -> IO StudioAppAuth
getAuthInfoData spec app = do
  auth <- AS.App.auth app
  let methodNames = enabledAuthMethodNames (AS.App.Auth.methods auth)
  return
    StudioAppAuth
      { userEntity = StudioEntity {name = fst (AS.resolveRef spec $ AS.App.Auth.userEntity auth)}
      , methods = methodNames
      }

