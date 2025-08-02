module Wasp.Cli.Command.CreateNewProject.AI
  ( createNewProjectInteractiveOnDisk,
    createNewProjectNonInteractiveOnDisk,
    createNewProjectNonInteractiveToStdout,
  )
where

import Control.Arrow ()
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', Rel, File', basename, fromAbsDir, fromRelDir)
import qualified StrongPath as SP
import StrongPath.Operations ()
import System.Directory (createDirectory, createDirectoryIfMissing, setCurrentDirectory)
import System.FilePath (takeDirectory)
import qualified System.FilePath as FP
import System.IO (hFlush, stdout)
import qualified Wasp.AI.CodeAgent as CA
import qualified Wasp.AI.GenerateNewProject as GNP
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectConfig,
    NewProjectDetails (..),
    emptyNewProjectConfig,
  )
import qualified Wasp.AI.GenerateNewProject.Common as GNP.C
import qualified Wasp.AI.GenerateNewProject.LogMsg as GNP.L
import Wasp.AI.Provider (Provider(..), ApiKey, getProviderApiKey)
import qualified Wasp.AI.OpenAI.ChatGPT as ChatGPT
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectAppName (..),
    obtainAvailableProjectDirPath,
    parseWaspProjectNameIntoAppName,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (readWaspProjectSkeletonFiles)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util as U
import qualified Wasp.Util.Aeson as Utils.Aeson
import qualified Wasp.Util.Terminal as T

createNewProjectInteractiveOnDisk :: Provider -> Path' Abs (Dir WaspProjectDir) -> NewProjectAppName -> Command ()
createNewProjectInteractiveOnDisk provider waspProjectDir appName = do
  apiKey <- getProviderApiKey provider
  appDescription <- liftIO $ Interactive.askForRequiredInput "Describe your app in a couple of sentences"
  (planningGptModel, codingGptModel) <-
    liftIO $
      Interactive.askToChoose'
        "Choose GPT model(s) you want to use:"
        $ NE.fromList
          [ Interactive.Option
              "gpt-4o (planning + coding)"
              (Just "Good results. Cheap and fast. Best cost/benefit ratio.")
              (ChatGPT.GPT_4o, ChatGPT.GPT_4o),
            Interactive.Option
              "gpt-4 (planning) + gpt-4o (coding)"
              (Just "Possibly better results, but somewhat slower and somewhat more expensive.")
              (ChatGPT.GPT_4, ChatGPT.GPT_4o),
            Interactive.Option
              "gpt-4 (planning + coding)"
              (Just "Possibly best results, but quite slower and quite more expensive.")
              (ChatGPT.GPT_4, ChatGPT.GPT_4)
          ]
  temperature <-
    liftIO $
      Interactive.askToChoose'
        "Choose the creativity level (temperature):"
        $ NE.fromList
          [ Interactive.Option
              "Balanced (0.7)"
              (Just "Optimal trade-off between creativity and possible mistakes.")
              0.7,
            Interactive.Option
              "Conventional (0.4)"
              (Just "Generates sensible code with minimal amount of mistakes.")
              0.4,
            Interactive.Option
              "Creative (1.0)"
              (Just "Generates more creative code, but mistakes are more likely.")
              1.0
          ]
  let projectConfig =
        emptyNewProjectConfig
          { GNP.C.projectPlanningGptModel = Just planningGptModel,
            GNP.C.projectCodingGptModel = Just codingGptModel,
            GNP.C.projectDefaultGptTemperature = Just temperature
          }

  liftIO $ createNewProjectOnDisk provider apiKey waspProjectDir appName appDescription projectConfig

  liftIO $ do
    putStrLn $
      unlines
        [ "",
          "========",
          "",
          "⚠️  Experimental tech",
          "Since this is a GPT generated app, it will likely contain some mistakes, proportional to how",
          "complex the app is. If there are some in your app, check out Wasp docs for help while",
          "fixing them, and also feel free to reach out to us on Discord! You can also try",
          "generating the app again to get different results (try playing with the creativity level).",
          " - Wasp docs: https://wasp.sh/docs",
          " - Our Discord: https://discord.gg/rzdnErX",
          "",
          "========"
        ]

createNewProjectNonInteractiveOnDisk :: Provider -> String -> String -> String -> Command ()
createNewProjectNonInteractiveOnDisk provider projectName appDescription projectConfigJson = do
  appName <- case parseWaspProjectNameIntoAppName projectName of
    Right appName -> pure appName
    Left err -> throwError $ CommandError "Invalid project name" err
  projectConfig <-
    Utils.Aeson.decodeFromString projectConfigJson
      & either (throwError . CommandError "Invalid project config" . ("Failed to parse JSON: " <>)) pure
  waspProjectDir <- obtainAvailableProjectDirPath projectName
  apiKey <- getProviderApiKey provider
  liftIO $ createNewProjectOnDisk provider apiKey waspProjectDir appName appDescription projectConfig

createNewProjectOnDisk ::
  Provider ->
  ApiKey ->
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectAppName ->
  String ->
  NewProjectConfig ->
  IO ()
createNewProjectOnDisk provider apiKey waspProjectDir appName appDescription projectConfig = do
  createDirectory $ fromAbsDir waspProjectDir
  setCurrentDirectory $ fromAbsDir waspProjectDir
  generateNewProject codeAgentConfig appName appDescription projectConfig
  where
    codeAgentConfig =
      CA.CodeAgentConfig
        { CA._llmProvider = ChatGPT.OpenAIProvider apiKey,
          CA._writeFile = writeFileToDisk,
          CA._writeLog = forwardLogToStdout
        }

    writeFileToDisk :: SP.Path' (SP.Rel WaspProjectDir) SP.File' -> T.Text -> IO ()
    writeFileToDisk path content = do
      let fp = SP.toFilePath path
      createDirectoryIfMissing True (takeDirectory fp)
      T.IO.writeFile fp content
      putStrLn $ T.applyStyles [T.Yellow] $ "> Wrote to file: " <> fromRelDir (basename waspProjectDir) FP.</> fp
      hFlush stdout

    forwardLogToStdout :: GNP.L.LogMsg -> IO ()
    forwardLogToStdout msg = do
      putStrLn $ GNP.L.toTermString msg
      hFlush stdout

-- | Instead of writing files to disk, it will write files (and logs) to the stdout,
-- with delimiters that make it easy to programmaticaly parse the output.
createNewProjectNonInteractiveToStdout :: Provider -> String -> String -> String -> Command ()
createNewProjectNonInteractiveToStdout provider projectName appDescription projectConfigJsonStr = do
  apiKey <- getProviderApiKey provider

  appName <- case parseWaspProjectNameIntoAppName projectName of
    Right appName -> pure appName
    Left err -> throwError $ CommandError "Invalid project name" err

  projectConfig <-
    Utils.Aeson.decodeFromString projectConfigJsonStr
      & either (throwError . CommandError "Invalid project config" . ("Failed to parse JSON: " <>)) pure

  let codeAgentConfig =
        CA.CodeAgentConfig
          { CA._llmProvider = ChatGPT.OpenAIProvider apiKey,
            CA._writeFile = writeFileToStdoutWithDelimiters,
            CA._writeLog = writeLogToStdoutWithDelimiters
          }

  liftIO $ generateNewProject codeAgentConfig appName appDescription projectConfig
  where
    writeFileToStdoutWithDelimiters :: SP.Path' (SP.Rel WaspProjectDir) SP.File' -> T.Text -> IO ()
    writeFileToStdoutWithDelimiters path content =
      writeToStdoutWithDelimiters "WRITE FILE" [SP.toFilePath path, T.unpack content]

    writeLogToStdoutWithDelimiters :: GNP.L.LogMsg -> IO ()
    writeLogToStdoutWithDelimiters msg =
      unless (null msg') $
        writeToStdoutWithDelimiters "LOG" [msg']
      where
        msg' = U.trim $ GNP.L.toPlainString msg

    writeToStdoutWithDelimiters :: String -> [String] -> IO ()
    writeToStdoutWithDelimiters delimiterTitle paragraphs = do
      putStrLn . ("\n" <>) $ withDelimiter delimiterTitle $ intercalate "\n" paragraphs
      hFlush stdout

    withDelimiter :: String -> String -> String
    withDelimiter title content =
      intercalate
        "\n"
        [ "==== WASP AI: " <> title <> " ====",
          content,
          "===/ WASP AI: " <> title <> " ===="
        ]

generateNewProject :: CA.CodeAgentConfig GNP.L.LogMsg -> NewProjectAppName -> String -> NewProjectConfig -> IO ()
generateNewProject codeAgentConfig (NewProjectAppName appName) appDescription projectConfig = do
  waspProjectSkeletonFiles <- readWaspProjectSkeletonFiles
  CA.runCodeAgent codeAgentConfig $ do
    GNP.generateNewProject (newProjectDetails projectConfig appName appDescription) waspProjectSkeletonFiles


newProjectDetails :: NewProjectConfig -> String -> String -> NewProjectDetails
newProjectDetails projectConfig webAppName webAppDescription =
  NewProjectDetails
    { _projectAppName = webAppName,
      _projectDescription = webAppDescription,
      _projectConfig = projectConfig
    }
