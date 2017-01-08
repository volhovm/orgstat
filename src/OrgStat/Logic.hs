{-# LANGUAGE ScopedTypeVariables #-}

-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import           Control.Lens                (view, (.~), _3)
import           Data.List                   (notElem, nub, nubBy)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime (..), addDays, defaultTimeLocale,
                                              formatTime, getCurrentTime, getZonedTime,
                                              toGregorian)
import           Data.Time.Calendar          (addGregorianMonthsRollOver)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             ((</>))
import           System.Wlog                 (logDebug, logInfo)
import           Universum
import           Unsafe                      (unsafeHead)

import           OrgStat.Ast                 (Org (..), mergeClocks, orgTitle)
import           OrgStat.Config              (ConfDate (..), ConfRange (..),
                                              ConfReport (..), ConfReportType (..),
                                              ConfScope (..), ConfigException (..),
                                              OrgStatConfig (..))
import           OrgStat.IO                  (readConfig, readOrgFile)
import           OrgStat.Report              (processTimeline, tpColorSalt, writeReport)
import           OrgStat.Util                (fromJustM)
import           OrgStat.WorkMonad           (WorkM, wConfigFile)


-- Converts config range to a pair of 'UTCTime', right bound not inclusive.
convertRange :: (MonadIO m) => ConfRange -> m (UTCTime, UTCTime)
convertRange range = case range of
    (ConfFromTo f t)  -> (,) <$> fromConfDate f <*> fromConfDate t
    (ConfBlockDay i) | i < 0 -> panic $ "ConfBlockDay i is <0: " <> show i
    (ConfBlockDay 0) -> (,) <$> (utcFromDay <$> startOfDay) <*> curTime
    (ConfBlockDay i) -> do
        d <- (negate (i - 1) `addDays`) <$> startOfDay
        pure $ utcFromDayPair ((negate 1) `addDays` d, d)
    (ConfBlockWeek i) | i < 0 -> panic $ "ConfBlockWeek i is <0: " <> show i
    (ConfBlockWeek 0) -> (,) <$> (utcFromDay <$> startOfWeek) <*> curTime
    (ConfBlockWeek i) -> do
        d <- (negate (i - 1) `addWeeks`) <$> startOfWeek
        pure $ utcFromDayPair ((negate 1) `addWeeks` d, d)
    (ConfBlockMonth i) | i < 0 -> panic $ "ConfBlockMonth i is <0: " <> show i
    (ConfBlockMonth 0) -> (,) <$> (utcFromDay <$> startOfMonth) <*> curTime
    (ConfBlockMonth i) -> do
        d <- addGregorianMonthsRollOver (negate $ i-1) <$> startOfMonth
        pure $ utcFromDayPair ((negate 1) `addGregorianMonthsRollOver` d, d)
  where
    utcFromDay d = UTCTime d 0
    utcFromDayPair = bimap utcFromDay utcFromDay
    curTime = liftIO getCurrentTime
    curDay = utctDay <$> curTime
    addWeeks i d = (i*7) `addDays` d
    startOfDay = curDay
    startOfWeek = do
        d <- curDay
        let weekDay = pred $ view _3 $ toWeekDate d
        pure $ fromIntegral (negate weekDay) `addDays` d
    startOfMonth = do
        d <- curDay
        let monthDate = pred $ view _3 $ toGregorian d
        pure $ fromIntegral (negate monthDate) `addDays` d
    fromConfDate ConfNow     = liftIO getCurrentTime
    fromConfDate (ConfUTC x) = pure x


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
            (from,to) <- convertRange timelineRange
            res <- processTimeline timelineParamsFinal orgTop (from,to)
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
