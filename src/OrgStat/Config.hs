{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Configuration file types, together with json instances.

module OrgStat.Config
       ( ConfigException (..)
       , ConfDate(..)
       , ConfRange(..)
       , ConfReportType(..)
       , ConfScope (..)
       , ConfReport (..)
       , OrgStatConfig (..)
       ) where

import           Data.Aeson               (FromJSON (..), Value (Object, String), (.!=),
                                           (.:), (.:?))
import           Data.Aeson.Types         (typeMismatch)
import           Data.Default             (def)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.Text                as T
import           Data.Time                (LocalTime)
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)
import           Universum

import           OrgStat.Outputs.Timeline (TimelineParams, tpBackground, tpColumnHeight,
                                           tpColumnWidth, tpLegend, tpTopDay)
import           OrgStat.Scope            (AstPath (..), ScopeModifier (..))
import           OrgStat.Util             (parseColour, (??~))

-- | Exception type for everything bad that happens with config,
-- starting from parsing to logic errors.
data ConfigException
    = ConfigParseException Text
    | ConfigLogicException Text
    deriving (Show, Typeable)

instance Exception ConfigException

data ConfDate
    = ConfNow
    | ConfLocal !LocalTime
    deriving (Show)

data ConfRange
    = ConfFromTo !ConfDate !ConfDate
    | ConfBlockWeek !Integer
    | ConfBlockDay !Integer
    | ConfBlockMonth !Integer
    deriving (Show)

data ConfReportType = Timeline
    { timelineRange  :: !ConfRange
    , timelineScope  :: !Text
    , timelineParams :: !TimelineParams
    } deriving (Show)

data ConfScope = ConfScope
    { csName  :: !Text              -- default is "default"
    , csPaths :: !(NonEmpty FilePath)
    } deriving (Show)

data ConfReport = ConfReport
    { crType      :: !ConfReportType -- includes config
    , crName      :: !Text
    , crModifiers :: ![ScopeModifier]
    } deriving (Show)

data OrgStatConfig = OrgStatConfig
    { confScopes             :: ![ConfScope]
    , confReports            :: ![ConfReport]
    , confBaseTimelineParams :: !TimelineParams
    , confTodoKeywords       :: ![Text]
    , confOutputDir          :: !FilePath -- default is "./orgstat"
    , confColorSalt          :: !Int
    } deriving (Show)

instance FromJSON AstPath where
    parseJSON (String s) = pure $ AstPath $ filter (not . T.null) $ T.splitOn "/" s
    parseJSON invalid    = typeMismatch "AstPath" invalid

instance FromJSON ScopeModifier where
    parseJSON (Object v) = do
        v .: "type" >>= \case
            (String "prune") -> ModPruneSubtree <$> v .: "path" <*> v .:? "depth" .!= 0
            (String "select") -> ModSelectSubtree <$> v .: "path"
            other -> fail $ "Unsupported scope modifier type: " ++ show other
    parseJSON invalid    = typeMismatch "ScopeModifier" invalid

instance FromJSON ConfDate where
    parseJSON (String "now") = pure $ ConfNow
    parseJSON (String s)     =
        case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" (T.unpack s) of
            Nothing -> fail $
                "Couldn't read date " <> show s <>
                ". Correct format is 2016-01-01 23:59"
            Just ut -> pure $ ConfLocal ut
    parseJSON invalid        = typeMismatch "ConfDate" invalid

instance FromJSON ConfRange where
    parseJSON (String s) | any (`T.isPrefixOf` s) ["day", "week", "month"] = do
        let splitted = T.splitOn "-" $ if "-" `T.isInfixOf` s then s else s <> "-"
            [range, number] = splitted
            constructor :: (Monad m, MonadFail m) => Integer -> m ConfRange
            constructor i = case range of
                "day"   -> pure $ ConfBlockDay i
                "week"  -> pure $ ConfBlockWeek i
                "month" -> pure $ ConfBlockMonth i
                t       -> fail $ "ConfRange@parseJSON can't parse " <> T.unpack t <>
                                  " should be [day|week|month]"
            numberParsed
                | number == "" = pure 0
                | otherwise = case readMaybe (T.unpack number) of
                    Nothing -> fail $ "Couldn't parse number modifier of " <> T.unpack s
                    Just x  -> pure x
        when (length splitted /= 2) $
            fail $ "Couldn't parse range " <> T.unpack s <>
                   ", splitted is " <> show splitted
        constructor =<< numberParsed
    parseJSON (Object v)       = ConfFromTo <$> v .: "from" <*> v .: "to"
    parseJSON invalid          = typeMismatch "ConfRange" invalid

instance FromJSON TimelineParams where
    parseJSON (Object v) = do
        legend <- v .:? "legend"
        topDay <- v .:? "topDay"
        colWidth <- v .:? "colWidth"
        colHeight <- v .:? "colHeight"
        bgColorRaw <- v .:? "background"
        pure $ def & tpLegend ??~ legend
                   & tpTopDay ??~ topDay
                   & tpColumnWidth ??~ colWidth
                   & tpColumnHeight ??~ colHeight
                   & tpBackground ??~ (T.strip <$> bgColorRaw >>= parseColour @Text)
    parseJSON invalid    = typeMismatch "TimelineParams" invalid

instance FromJSON ConfReportType where
    parseJSON o@(Object v) = do
        v .: "type" >>= \case
            (String "timeline") ->
                Timeline <$> v .: "range"
                         <*> v .:? "scope" .!= "default"
                         <*> parseJSON o
            other -> fail $ "Unsupported scope modifier type: " ++ show other
    parseJSON invalid    = typeMismatch "ConfReportType" invalid

instance FromJSON ConfScope where
    parseJSON (Object v) = do
        ConfScope <$> v .:? "name" .!= "default" <*> v .: "paths"
    parseJSON invalid    = typeMismatch "ConfScope" invalid

instance FromJSON ConfReport where
    parseJSON (Object v) = do
        ConfReport <$> v .: "type"
                   <*> v .:? "name" .!= "default"
                   <*> v .:? "modifiers" .!= []
    parseJSON invalid    = typeMismatch "ConfReport" invalid

instance FromJSON OrgStatConfig where
    parseJSON (Object v) = do
        OrgStatConfig <$> v .: "scopes"
                      <*> v .: "reports"
                      <*> v .:? "timelineDefault" .!= def
                      <*> v .:? "todoKeywords" .!= []
                      <*> v .:? "output" .!= "./orgstat"
                      <*> v .:? "colorSalt" .!= 0
    parseJSON invalid    = typeMismatch "ConfReport" invalid
