module Wasp.Cli.Command.Studio
  ( studio,
    makeAppInfoJson,
    generateStudioDataFile,
  )
where

import Control.Arrow ()
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
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
makeAppInfoJson waspDir spec = do
  let (appName, app) = ASV.getApp spec
  opsRes <- StudioOps.collectPageOperations waspDir spec
  let pageOps = either (const M.empty) id opsRes
  return $
    object
      [ "pages"
          .= map
            ( \(name, page) ->
                object
                  [ "name" .= name,
                    "authRequired" .= AS.Page.authRequired page,
                    "operations" .= M.lookupDefault [] name pageOps
                  ]
            )
            (AS.getPages spec),
        "routes"
          .= map
            ( \(name, route) ->
                object
                  [ "name" .= name,
                    "path" .= AS.Route.path route,
                    "toPage" .= object ["name" .= fst (AS.resolveRef spec $ AS.Route.to route)]
                  ]
            )
            (AS.getRoutes spec),
        "apis"
          .= map
            ( \(name, api) ->
                object
                  [ "name" .= name,
                    "httpRoute" .=
                      let (method, path) = AS.Api.httpRoute api
                       in object ["method" .= show method, "path" .= path],
                    "auth" .= AS.Api.auth api,
                    "entities" .= getLinkedEntitiesData spec (AS.Api.entities api)
                  ]
            )
            (AS.getApis spec),
        "jobs"
          .= map
            ( \(name, job) ->
                object
                  [ "name" .= name,
                    "schedule" .= (AS.Job.cron <$> AS.Job.schedule job),
                    "entities" .= getLinkedEntitiesData spec (AS.Job.entities job)
                  ]
            )
            (AS.getJobs spec),
        "operations"
          .= map
            ( \operation ->
                object
                  [ "type" .= case operation of
                      _op@(QueryOp _ _) -> "query" :: String
                      _op@(ActionOp _ _) -> "action",
                    "name" .= Operation.getName operation,
                    "entities" .= getLinkedEntitiesData spec (Operation.getEntities operation),
                    "auth" .= Operation.getAuth operation
                  ]
            )
            (AS.getOperations spec),
        "cruds"
          .= map
            ( \(name, crud) ->
                object
                  [ "name" .= name,
                    "operations"
                      .= map (show . fst) (crudDeclarationToOperationsList crud),
                    "entities" .= getLinkedEntitiesData spec (Just [AS.Crud.entity crud])
                  ]
            )
            (AS.getCruds spec),
        "entities"
          .= map
            ( \(name, _entity) -> object ["name" .= name] )
            (AS.getEntities spec),
        "app"
          .= object
            [ "name" .= (appName :: String),
              "auth" .= getAuthInfo spec app,
              "db" .= getDbInfo spec
            ]
      ]

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

getLinkedEntitiesData :: AS.AppSpec -> Maybe [AS.Ref AS.Entity] -> [Data.Aeson.Value]
getLinkedEntitiesData spec entityRefs =
  map (\(entityName, _entity) -> object ["name" .= entityName]) $
    resolveEntities spec entityRefs

resolveEntities :: AS.AppSpec -> Maybe [AS.Ref AS.Entity] -> [(String, AS.Entity)]
resolveEntities spec entityRefs = AS.resolveRef spec <$> fromMaybe [] entityRefs

getDbInfo :: AS.AppSpec -> Value
getDbInfo spec = object ["system" .= show (ASV.getValidDbSystem spec)]

getAuthInfo :: AS.AppSpec -> AS.App.App -> IO Value
getAuthInfo spec app = do
  auth <- AS.App.auth app
  let methodNames = enabledAuthMethodNames (AS.App.Auth.methods auth)
  return $
    object
      [ "userEntity" .= object ["name" .= fst (AS.resolveRef spec $ AS.App.Auth.userEntity auth)],
        "methods" .= methodNames
      ]

