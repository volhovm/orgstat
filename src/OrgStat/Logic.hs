-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import           Control.Lens        (view, (.~))
import           Data.Default        (def)
import qualified Data.List.NonEmpty  as NE
import qualified Data.Text           as T
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (getZonedTime)
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     ((</>))
import           System.Wlog         (logDebug, logInfo)
import           Universum

import           OrgStat.Ast         (Org (..), orgTitle)
import           OrgStat.Config      (ConfReport (..), ConfReportType (..),
                                      ConfScope (..), ConfigException (..),
                                      OrgStatConfig (..))
import           OrgStat.IO          (readConfig, readOrgFile)
import           OrgStat.Report      (processTimeline, writeReport)
import           OrgStat.Util        (fromJustM)
import           OrgStat.WorkMonad   (WorkM, wConfigFile)

runOrgStat :: WorkM ()
runOrgStat = do
    config@OrgStatConfig{..} <- readConfig =<< view wConfigFile
    logDebug $ "Config: \n" <> show config

    curTime <- liftIO getZonedTime
    let reportDir = confOutputDir </> formatTime defaultTimeLocale "%F-%H-%M-%S" curTime
    liftIO $ createDirectoryIfMissing True reportDir
    logInfo $ "This report set will be put into: " <> T.pack reportDir

    let getScope scopeName reportName =
            fromJustM (throwLogic $ scopeNotFound scopeName reportName) $
            pure $ find ((== scopeName) . csName) confScopes
    forM_ confReports $ \ConfReport{..} -> case crType of
        Timeline _range scopeName -> do
            logDebug $ "Processing report " <> crName
            scopeFiles <- getScope scopeName crName
            parsedOrgs <-
                mapM (readOrgFile confTodoKeywords) (NE.toList $ csPaths scopeFiles)
            let orgTop = Org "/" [] [] $ map (\(fn,o) -> o & orgTitle .~ fn) parsedOrgs
            res <- processTimeline def orgTop
            logInfo $ "Generating report " <> crName <> "..."
            writeReport reportDir (T.unpack crName) res
  where
    scopeNotFound scope report =
        mconcat ["Scope ", scope, " is requested for config report ",
                 report, ", but is not present in scopes section"]
    throwLogic = throwM . ConfigLogicException
