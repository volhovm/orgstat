{-# LANGUAGE ScopedTypeVariables #-}

-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import           Control.Lens        (view, (.~))
import           Data.List           (notElem, nub, nubBy)
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map            as M
import qualified Data.Text           as T
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (getZonedTime)
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     ((</>))
import           System.Wlog         (logDebug, logInfo)
import           Universum
import           Unsafe              (unsafeHead)

import           OrgStat.Ast         (Org (..), mergeClocks, orgTitle)
import           OrgStat.Config      (ConfReport (..), ConfReportType (..),
                                      ConfScope (..), ConfigException (..),
                                      OrgStatConfig (..))
import           OrgStat.IO          (readConfig, readOrgFile)
import           OrgStat.Report      (processTimeline, tpColorSalt, writeReport)
import           OrgStat.Util        (fromJustM)
import           OrgStat.WorkMonad   (WorkM, wConfigFile)

runOrgStat :: WorkM ()
runOrgStat = do
    conf@OrgStatConfig{..} <- readConfig =<< view wConfigFile
    logDebug $ "Config: \n" <> show conf

    curTime <- liftIO getZonedTime
    let reportDir = confOutputDir </> formatTime defaultTimeLocale "%F-%H-%M-%S" curTime
    liftIO $ createDirectoryIfMissing True reportDir
    logInfo $ "This report set will be put into: " <> T.pack reportDir

    allParsedOrgs <- parseNeededFiles conf
    forM_ confReports $ \ConfReport{..} -> case crType of
        Timeline {..} -> do
            logDebug $ "Processing report " <> crName
            scope <- getScope conf timelineScope crName
            let scopeFiles = NE.toList $ csPaths scope
                neededOrgs =
                    map (\f -> fromMaybe (panic $ scopeNotFound (T.pack f) crName) $
                               M.lookup f allParsedOrgs)
                        scopeFiles
            let orgTop =
                    mergeClocks $
                    Org "/" [] [] $ map (\(fn,o) -> o & orgTitle .~ fn) neededOrgs
            let timelineParamsFinal = timelineParams & tpColorSalt .~ confColorSalt
            res <- processTimeline timelineParamsFinal orgTop undefined
            logInfo $ "Generating report " <> crName <> "..."
            writeReport reportDir (T.unpack crName) res
  where
    scopeNotFound scope report =
        mconcat ["Scope ", scope, " is requested for config report ",
                 report, ", but is not present in scopes section"]
    throwLogic = throwM . ConfigLogicException
    getScope OrgStatConfig{..} scopeName reportName =
        fromJustM (throwLogic $ scopeNotFound scopeName reportName) $
        pure $ find ((== scopeName) . csName) confScopes
    getScopes (Timeline _ s _) = [s]
    -- Reads needed org files and returns a map
    parseNeededFiles :: OrgStatConfig -> WorkM (Map FilePath (Text, Org))
    parseNeededFiles conf@OrgStatConfig{..} = do
        let neededScopes =
                nubBy ((==) `on` snd) $
                concatMap (\cr -> map (crName cr,) $ getScopes (crType cr)) confReports
        let availableScopes = map csName confScopes
        let notAvailableScopes = filter (\(_,s) -> s `notElem` availableScopes) neededScopes
        unless (null notAvailableScopes) $ do
            let (r,s) = unsafeHead notAvailableScopes
            throwLogic $ scopeNotFound s r
        neededFiles <-
            nub . concatMap (NE.toList . csPaths) <$>
            mapM (\(r,s) -> getScope conf s r) neededScopes
        fmap M.fromList $ forM neededFiles (\f -> (f,) <$> readOrgFile confTodoKeywords f)
