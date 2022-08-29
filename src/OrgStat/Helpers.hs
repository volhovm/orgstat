-- | Different logic-related components.

module OrgStat.Helpers
       ( convertRange
       , resolveInputOrg
       , resolveScope
       , resolveReport
       , resolveOutput
       ) where

import Universum

import Control.Lens (at, views, (.=))
import qualified Data.List.NonEmpty as NE
import Data.Time
  (LocalTime(..), TimeOfDay(..), addDays, getZonedTime, toGregorian, zonedTimeToLocalTime)
import Data.Time.Calendar (addGregorianMonthsRollOver)
import Data.Time.Calendar.WeekDate (toWeekDate)

import OrgStat.Ast (Org(..), cutFromTo, orgTitle, Title(..))
import OrgStat.Config
  (ConfDate(..), ConfOutput(..), ConfRange(..), ConfReport(..), ConfScope(..), ConfigException(..),
  OrgStatConfig(..))
import OrgStat.IO (readOrgFile)
import OrgStat.Scope (applyModifiers)
import OrgStat.WorkMonad (WorkM, wcConfig, wdReadFiles, wdResolvedReports, wdResolvedScopes)


-- | Converts config range to a pair of 'UTCTime', right bound not inclusive.
convertRange :: (MonadIO m) => ConfRange -> m (LocalTime, LocalTime)
convertRange range = case range of
    (ConfFromTo f t)  -> (,) <$> fromConfDate f <*> fromConfDate t
    (ConfBlockDay i) | i < 0 -> error $ "ConfBlockDay i is <0: " <> show i
    (ConfBlockDay 0) -> (,) <$> (localFromDay <$> startOfDay) <*> curTime
    (ConfBlockDay i) -> do
        d <- (negate (i - 1) `addDays`) <$> startOfDay
        pure $ localFromDayPair ((negate 1) `addDays` d, d)
    (ConfBlockWeek i) | i < 0 -> error $ "ConfBlockWeek i is <0: " <> show i
    (ConfBlockWeek 0) -> (,) <$> (localFromDay <$> startOfWeek) <*> curTime
    (ConfBlockWeek i) -> do
        d <- (negate (i - 1) `addWeeks`) <$> startOfWeek
        pure $ localFromDayPair ((negate 1) `addWeeks` d, d)
    (ConfBlockMonth i) | i < 0 -> error $ "ConfBlockMonth i is <0: " <> show i
    (ConfBlockMonth 0) -> (,) <$> (localFromDay <$> startOfMonth) <*> curTime
    (ConfBlockMonth i) -> do
        d <- addGregorianMonthsRollOver (negate $ i-1) <$> startOfMonth
        pure $ localFromDayPair ((negate 1) `addGregorianMonthsRollOver` d, d)
  where
    localFromDay d = LocalTime d $ TimeOfDay 0 0 0
    localFromDayPair = bimap localFromDay localFromDay
    curTime = liftIO $ zonedTimeToLocalTime <$> getZonedTime
    curDay = localDay <$> curTime
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
    fromConfDate ConfNow       = curTime
    fromConfDate (ConfLocal x) = pure x

-- | Resolves org file: reads from path and puts into state or just
-- gets out of state if was read before.
resolveInputOrg :: FilePath -> WorkM (Text, Org)
resolveInputOrg fp = use (wdReadFiles . at fp) >>= \case
    Just x -> pure x
    Nothing -> do
        todoKeywords <- views wcConfig confTodoKeywords
        o <- readOrgFile todoKeywords fp
        wdReadFiles . at fp .= Just o
        pure o

-- A lot of copy-paste here... 2 bad, though no time to fix

-- | Return scope with requested name or fail. It will be either
-- constructed on the spot or taken from the state if it had been
-- created previously.
resolveScope :: Text -> WorkM Org
resolveScope scopeName = use (wdResolvedScopes . at scopeName) >>= \case
    Just x -> pure x
    Nothing -> constructScope
  where
    constructScope = do
        let filterScopes = filter (\x -> csName x == scopeName)
        views wcConfig (filterScopes . confScopes) >>= \case
            [] ->
                throwM $ ConfigLogicException $
                "Scope "<> scopeName <> " is not declared"
            [sc] -> resolveFoundScope sc
            scopes ->
                throwM $ ConfigLogicException $
                "Multple scopes with name "<> scopeName <>
                " are declared " <> show scopes
    resolveFoundScope ConfScope{..} = do
        orgs <- NE.toList <$> forM csPaths resolveInputOrg
        let orgTop = Org (Title "/") [] [] $ map (\(fn,o) -> o & orgTitle .~ Title fn) orgs
        wdResolvedScopes . at scopeName .= Just orgTop
        pure orgTop

-- | Same as resolveScope but related to reports.
resolveReport :: Text -> WorkM Org
resolveReport reportName = use (wdResolvedReports . at reportName) >>= \case
    Just x -> pure x
    Nothing -> constructReport
  where
    constructReport = do
        let filterReports = filter (\x -> crName x == reportName)
        views wcConfig (filterReports . confReports) >>= \case
            [] ->
                throwM $ ConfigLogicException $
                "Report " <> reportName <> " is not declared"
            [rep] -> resolveFoundReport rep
            reports ->
                 throwM $ ConfigLogicException $
                "Multple reports with name "<> reportName <>
                " are declared " <> show reports
    resolveFoundReport ConfReport{..} = do
        orgTop <- resolveScope crScope
        fromto <- convertRange crRange
        withModifiers <-
            either throwM pure $
            applyModifiers orgTop crModifiers
        --let finalOrg = cutFromTo fromto $ mergeClocks withModifiers
        let finalOrg = cutFromTo fromto withModifiers
        wdResolvedReports . at reportName .= Just finalOrg
        pure finalOrg

resolveOutput :: Text -> WorkM ConfOutput
resolveOutput outputName =
    views wcConfig (filterOutputs . confOutputs) >>= \case
        [] ->
            throwM $ ConfigLogicException $
            "Report " <> outputName <> " is not declared"
        [rep] -> pure rep
        reports ->
             throwM $ ConfigLogicException $
            "Multple outputs with name "<> outputName <>
            " are declared " <> show reports
  where
    filterOutputs = filter (\x -> coName x == outputName)
