-- | Script output type. We launch the executable asked after
-- injecting the environment variables related to the report.

module OrgStat.Outputs.Script
       ( processScriptOutput
       ) where

import Universum

import Control.Lens (views)
import qualified Data.Map.Strict as M
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Process (callCommand)

import OrgStat.Ast (filterHasClock, orgTotalDuration)
import OrgStat.Config (confReports, crName)
import OrgStat.Helpers (resolveReport)
import OrgStat.Outputs.Types (ScriptParams (..))
import OrgStat.Util (timeF)
import OrgStat.WorkMonad (WorkM, wcConfig)

-- | Processes script output.
processScriptOutput :: ScriptParams -> WorkM ()
processScriptOutput ScriptParams{..} = do
    -- Considering all the reports if none are specified.
    reportsToConsider <- case spReports of
          [] -> views wcConfig (map crName . confReports)
          xs -> pure xs
    allReports <- mapM (\r -> (r,) <$> resolveReport r) reportsToConsider

    -- Set env variables
    prevVars <- forM allReports $ \(toString -> reportName,org) -> do
        let duration = timeF $ orgTotalDuration $ filterHasClock org
        (prevVar :: Maybe String) <- liftIO $ lookupEnv reportName
        liftIO $ setEnv reportName (toString duration)
        pure $ (reportName,) <$> prevVar
    let prevVarsMap :: Map String String
        prevVarsMap = M.fromList $ catMaybes prevVars

    -- Execute script
    let cmdArgument = either id (\t -> "-c \"" <> toString t <> "\"") spScript
    liftIO $ callCommand $
        "sh " <> cmdArgument

    -- Restore the old variables, clean new.
    forM_ (map fst allReports) $ \(toString -> reportName) -> do
        liftIO $ case M.lookup reportName prevVarsMap of
            Nothing        -> unsetEnv reportName
            Just prevValue -> setEnv reportName prevValue
