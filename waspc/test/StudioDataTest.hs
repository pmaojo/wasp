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

newtype StudioData = StudioData {pages :: [StudioPage]} deriving (Generic, Show)
instance FromJSON StudioData

data StudioPage = StudioPage {name :: String, operations :: [String]} deriving (Generic, Show)
instance FromJSON StudioPage

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
            Just (StudioData ps) -> do
              let ops = fromMaybe [] $ operations <$> find ((== "MainPage") . name) ps
              ops `shouldBe` ["getTasks"]

