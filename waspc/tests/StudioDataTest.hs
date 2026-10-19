module StudioDataTest where

import Control.Monad (void)
import Data.Aeson (decode)
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
import Wasp.Studio.Data

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
            Just studioData -> do
              let ops =
                    fromMaybe [] $ operations <$> find ((== "MainPage") . name) (pages studioData)
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
            Just studioData -> do
              case find ((== "tasks") . name) (cruds studioData) of
                Nothing -> expectationFailure "tasks crud not found"
                Just c -> do
                  operations c `shouldBe` ["Get", "GetAll", "Create", "Update", "Delete"]
                  fmap name (entities c) `shouldBe` ["Task"]


