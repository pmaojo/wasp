module Cli.StudioTest where

import Control.Monad (void)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
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

spec_CliStudioTest :: Spec
spec_CliStudioTest =
  describe "wasp studio" $ do
    it "creates .wasp-studio-data.json with required keys" $
      withSystemTempDirectory "cli-studio-test" $ \tmp -> do
        let exampleDir = systemSPRoot SP.</> [SP.reldir|workspace/wasp/waspc/examples/todoApp|]
        let tmpDir = fromJust $ SP.parseAbsDir tmp
        IO.copyDirectory exampleDir tmpDir
        let opts = defaultCompileOptions tmpDir
        (appSpecOrErr, _) <- Analyze.analyzeWaspProject tmpDir opts
        case appSpecOrErr of
          Left errs -> expectationFailure $ show errs
          Right spec -> do
            dataFile <- generateStudioDataFile tmpDir spec
            fileExists <- IO.doesFileExist dataFile
            fileExists `shouldBe` True
            bs <- BSL.readFile (SP.fromAbsFile dataFile)
            case decode bs :: Maybe StudioData of
              Just _ -> return ()
              Nothing -> expectationFailure "failed to decode json"
