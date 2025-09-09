module Wasp.AI.GenerateNewProject.Entity
  ( writeEntitiesToPrismaFile,
    entityPlanToPrismaModelText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Wasp.AI.GenerateNewProject.Common (CodeAgent, writeToWaspFileEnd)
import StrongPath (Path', Rel, File')
import qualified StrongPath as SP
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan

writeEntitiesToPrismaFile :: Path' (Rel WaspProjectDir) File' -> [Plan.Entity] -> CodeAgent ()
writeEntitiesToPrismaFile prismaFilePath entityPlans = do
  writeToWaspFileEnd prismaFilePath $ "\n" <> modelsCode
  where
    modelsCode = T.intercalate "\n\n" $ entityPlanToPrismaModelText <$> entityPlans

entityPlanToPrismaModelText :: Plan.Entity -> Text
entityPlanToPrismaModelText plan =
  let name = T.pack $ Plan.entityName plan
      pslBody = T.pack $ Plan.entityBodyPsl plan
   in [trimming|
        model ${name} {
          ${pslBody}
        }
      |]

-- TODO: Add data Entity that contains waspDeclaration + entity plan.
