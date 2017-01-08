{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Configuration file parser, together with json instances.

module OrgStat.Config
       ( ConfigException (..)
       , ConfReportType(..)
       , ConfScope (..)
       , ConfReport (..)
       , OrgStatConfig (..)
       ) where

import           Data.Aeson              (FromJSON (..), Value (Object, String), (.!=),
                                          (.:), (.:?))
import           Data.Aeson.Types        (typeMismatch)
import           Data.Default            (def)
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.Text               as T
import           Data.Time.Format        (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime     (ZonedTime)
import           Universum

import           OrgStat.Report.Timeline (TimelineParams, tpColumnHeight, tpColumnWidth,
                                          tpLegend, tpTopDays)
import           OrgStat.Scope           (AstPath (..), ScopeModifier (..))
import           OrgStat.Util            ((??~))

-- | Exception type for everything bad that happens with config,
-- starting from parsing to logic errors.
data ConfigException
    = ConfigParseException Text
    | ConfigLogicException Text
    deriving (Show, Typeable)

instance Exception ConfigException

data ConfDate
    = ConfNow
    | ConfZoned ZonedTime
    deriving (Show)

data ConfRange
    = ConfFromTo ConfDate ConfDate
    | ConfBlockWeek Int
    | ConfBlockDay Int
    | ConfBlockMonth Int
    deriving (Show)

data ConfReportType = Timeline
    { timelineRange  :: ConfRange
    , timelineScope  :: Text
    , timelineParams :: TimelineParams
    } deriving (Show)

data ConfScope = ConfScope
    { csName  :: Text -- default needs no name
    , csPaths :: NonEmpty FilePath
    } deriving (Show)

data ConfReport = ConfReport
    { crType      :: ConfReportType -- includes config
    , crName      :: Text
    , crModifiers :: [ScopeModifier]
    } deriving (Show)

data OrgStatConfig = OrgStatConfig
    { confScopes       :: [ConfScope]
    , confReports      :: [ConfReport]
    , confTodoKeywords :: [Text]
    , confOutputDir    :: FilePath -- default is "./orgstat"
    , confColorSalt    :: Int
    } deriving (Show)

instance FromJSON AstPath where
    parseJSON (String s)
        | null s = fail "AstPath FromJson: empty string"
        | otherwise = pure $ AstPath $ T.splitOn "/" s
    parseJSON invalid = typeMismatch "AstPath" invalid

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
            Just zt -> pure $ ConfZoned zt
    parseJSON invalid        = typeMismatch "ConfDate" invalid

instance FromJSON ConfRange where
    parseJSON (String "day")   = pure $ ConfBlockDay 0 -- todo add "-N" modifiers
    parseJSON (String "week")  = pure $ ConfBlockWeek 0
    parseJSON (String "month") = pure $ ConfBlockMonth 0
    parseJSON (Object v)       = ConfFromTo <$> v .: "from" <*> v .: "to"
    parseJSON invalid          = typeMismatch "ConfRange" invalid

instance FromJSON TimelineParams where
    parseJSON (Object v) = do
        legend <- v .:? "legend"
        topDays <- v .:? "topDays"
        colWidth <- v .:? "colWidth"
        colHeight <- v .:? "colHeight"
        pure $ def & tpLegend ??~ legend
                   & tpTopDays ??~ topDays
                   & tpColumnWidth ??~ colWidth
                   & tpColumnHeight ??~ colHeight
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
                      <*> v .:? "todoKeywords" .!= []
                      <*> v .:? "output" .!= "./orgstat"
                      <*> v .:? "colorSalt" .!= 0
    parseJSON invalid    = typeMismatch "ConfReport" invalid
