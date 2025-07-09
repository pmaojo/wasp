module Wasp.Studio.PageOperations
  ( PageOperations,
    collectPageOperations,
  ) where

import Control.Monad (forM)
import qualified Data.HashMap.Strict as M
import qualified StrongPath as SP
import qualified System.Directory as Dir
import System.FilePath ((<.>), takeExtension, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import Wasp.Project.Common (WaspProjectDir, srcDirInWaspProjectDir)
import qualified Wasp.TypeScript.Inspect.Operations as TS

-- | Map from page name to list of operations referenced in that page.
type PageOperations = M.HashMap String [String]

collectPageOperations :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> AppSpec -> IO (Either String PageOperations)
collectPageOperations waspDir spec = do
  let srcDir = waspDir SP.</> srcDirInWaspProjectDir
  let tsconfig = waspDir SP.</> AS.srcTsConfigPath spec
  reqs <- forM (AS.getPages spec) $ \(name, page) -> do
    fp <- resolvePageFile srcDir (AS.ExtImport.path $ AS.Page.component page)
    let pathStr = SP.fromAbsFile fp
    return (name, TS.TsOperationsRequest {TS.filepaths = [pathStr], TS.tsconfig = Just $ SP.fromAbsFile tsconfig})
  let tsRequests = map snd reqs
  TS.getOperationsOfTsFiles tsRequests >>= \case
    Left err -> return $ Left err
    Right (TS.TsOperationsResponse resMap) -> do
      let pageOps = M.fromList $ flip map reqs $ \(name, r) ->
            let path = head (TS.filepaths r)
             in (name, M.findWithDefault [] path resMap)
      return $ Right pageOps

resolvePageFile :: SP.Path' SP.Abs (SP.Dir d) -> SP.Path' SP.Rel SP.File' -> IO (SP.Path' SP.Abs SP.File')
resolvePageFile srcDir relFile = do
  let base = SP.fromAbsDir srcDir </> SP.fromRelFile relFile
  if takeExtension base /= ""
    then parse base
    else tryExt ["ts", "tsx", "js", "jsx"]
  where
    parse p = maybe (error $ "Invalid path: " ++ p) return (SP.parseAbsFile p)
    tryExt [] = parse base
    tryExt (e:es) = do
      let p = base <.> e
      exists <- Dir.doesFileExist p
      if exists then parse p else tryExt es

