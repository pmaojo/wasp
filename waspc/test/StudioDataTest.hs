module StudioDataTest where

import Control.Monad (void)
import Data.Aeson (FromJSON, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BSL
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import qualified StrongPath as SP
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.Hspec
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Studio (generateStudioDataFile)
import qualified Wasp.Project.Analyze as Analyze
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util.IO as IO
import Fixtures (systemSPRoot)

data StudioData = StudioData
  { pages :: [StudioPage],
    cruds :: Maybe [StudioCrud]
  }
  deriving (Generic, Show)
instance FromJSON StudioData

data StudioPage = StudioPage {pageName :: String, operations :: [String]} deriving (Generic, Show)
instance FromJSON StudioPage

data StudioCrud = StudioCrud
  { crudName :: String,
    crudOperations :: [String],
    crudEntities :: [StudioEntity]
  }
  deriving (Generic, Show)
instance FromJSON StudioCrud

newtype StudioEntity = StudioEntity {entityName :: String}
  deriving (Generic, Show)
instance FromJSON StudioEntity

spec_StudioDataTest :: Spec
spec_StudioDataTest = describe "generateStudioDataFile" $ do
  it "detects useQuery operations in TodoApp" $
    withSystemTempDirectory "studio-test" $ \tmp -> do
      let exampleDir = systemSPRoot SP.</> [SP.reldir|workspace/wasp/examples/tutorials/TodoApp|]
      let tmpDir = fromJust $ SP.parseAbsDir tmp
      IO.copyDirectory exampleDir tmpDir
      let opts = defaultCompileOptions tmpDir
      (appSpecOrErr, _) <- Analyze.analyzeWaspProject tmpDir opts
      case appSpecOrErr of
        Left errs -> expectationFailure $ show errs
        Right spec -> do
          dataFile <- generateStudioDataFile tmpDir spec
          bs <- BSL.readFile (SP.fromAbsFile dataFile)
          case decode bs :: Maybe StudioData of
            Nothing -> expectationFailure "failed to decode json"
            Just (StudioData ps _) -> do
                let ops = fromMaybe [] $ operations <$> find ((== "MainPage") . pageName) ps
                ops `shouldBe` ["getTasks"]

  it "includes CRUD info for tasks" $
    withSystemTempDirectory "studio-crud-test" $ \tmp -> do
      let exampleDir = systemSPRoot SP.</> [SP.reldir|workspace/wasp/waspc/examples/todoApp|]
      let tmpDir = fromJust $ SP.parseAbsDir tmp
      IO.copyDirectory exampleDir tmpDir
      let opts = defaultCompileOptions tmpDir
      (appSpecOrErr, _) <- Analyze.analyzeWaspProject tmpDir opts
      case appSpecOrErr of
        Left errs -> expectationFailure $ show errs
        Right spec -> do
          dataFile <- generateStudioDataFile tmpDir spec
          bs <- BSL.readFile (SP.fromAbsFile dataFile)
          case decode bs :: Maybe StudioData of
            Nothing -> expectationFailure "failed to decode json"
            Just (StudioData _ (Just crs)) -> do
              let findCrud c = if crudName c == "tasks" then Just c else Nothing
              case find (\c -> crudName c == "tasks") crs of
                Nothing -> expectationFailure "tasks crud not found"
                Just c -> do
                  crudOperations c `shouldBe` ["Get","GetAll","Create","Update","Delete"]
                  fmap entityName (crudEntities c) `shouldBe` ["Task"]
            Just (StudioData _ Nothing) -> expectationFailure "cruds missing"


