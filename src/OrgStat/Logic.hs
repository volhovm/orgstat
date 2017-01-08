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
    config@OrgStatConfig{..} <- readConfig =<< view wConfigFile
    logDebug $ "Config: \n" <> show config

    curTime <- liftIO getZonedTime
    let reportDir = confOutputDir </> formatTime defaultTimeLocale "%F-%H-%M-%S" curTime
    liftIO $ createDirectoryIfMissing True reportDir
    logInfo $ "This report set will be put into: " <> T.pack reportDir

    let getScopes (Timeline _ s _) = [s]
    let neededScopes =
            nubBy ((==) `on` snd) $
            concatMap (\cr -> map (crName cr,) $ getScopes (crType cr)) confReports
    let availableScopes = map csName confScopes
    let notAvailableScopes = filter (\(_,s) -> s `notElem` availableScopes) neededScopes
    when (null notAvailableScopes) $ do
        let (r,s) = unsafeHead notAvailableScopes
        throwLogic $ scopeNotFound s r
    let getScope scopeName reportName =
            fromJustM (throwLogic $ scopeNotFound scopeName reportName) $
            pure $ find ((== scopeName) . csName) confScopes
    neededFiles <-
        nub . concatMap (NE.toList . csPaths) <$>
        mapM (uncurry getScope) neededScopes
    (allParsedOrgs :: Map FilePath (Text,Org)) <-
        fmap M.fromList $ forM neededFiles (\f -> (f,) <$> readOrgFile confTodoKeywords f)
    forM_ confReports $ \ConfReport{..} -> case crType of
        Timeline {..} -> do
            logDebug $ "Processing report " <> crName
            (scopeFiles :: [FilePath]) <- NE.toList . csPaths <$> getScope timelineScope crName
            let neededOrgs :: [(Text,Org)]
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
