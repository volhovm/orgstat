-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import           Control.Lens      (view)
import           Data.Yaml         (decode)
import           System.Wlog       (logDebug)
import           Universum

import           OrgStat.IO        (readConfig, readOrgFile)
import           OrgStat.WorkMonad (WorkM, wConfigFile)

runOrgStat :: WorkM ()
runOrgStat = do
    config <- readConfig =<< view wConfigFile
    logDebug $ "Config: \n" <> show config
    undefined
