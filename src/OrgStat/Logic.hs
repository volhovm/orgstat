-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import           Control.Lens       (view)
import qualified Data.List.NonEmpty as NE
import           System.Wlog        (logDebug)
import           Universum

import           OrgStat.Config     (ConfReport (..), ConfReportType (..), ConfScope (..),
                                     ConfigException (..), OrgStatConfig (..))
import           OrgStat.IO         (readConfig, readOrgFile)
import           OrgStat.Util       (fromJustM)
import           OrgStat.WorkMonad  (WorkM, wConfigFile)

runOrgStat :: WorkM ()
runOrgStat = do
    config@OrgStatConfig{..} <- readConfig =<< view wConfigFile
    logDebug $ "Config: \n" <> show config
    let getScope scopeName reportName =
            fromJustM (throwLogic $ scopeNotFound scopeName reportName) $
            pure $ find ((== scopeName) . csName) confScopes
    forM_ confReports $ \ConfReport{..} -> case crType of
        Timeline _range scopeName -> do
            logDebug $ "Processing report " <> crName
            scopeFiles <- getScope scopeName crName
            subjs <- mapM readOrgFile (NE.toList $ csPaths scopeFiles)
            logDebug $ "Successfully read scopes: " <> show (map fst subjs)
            undefined
    undefined
  where
    scopeNotFound scope report =
        mconcat ["Scope ", scope, " is requested for config report ",
                 report, ", but is not present in scopes section"]
    throwLogic = throwM . ConfigLogicException
