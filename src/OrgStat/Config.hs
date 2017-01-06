{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Configuration file parser, together with json instances.

module OrgStat.Config
       ( ConfReportType(..)
       , ConfScope (..)
       , ConfReport (..)
       , OrgStatConfig (..)
       ) where

import           Data.Aeson          (FromJSON (..), Value (Object, String), (.!=), (.:))
import           Data.Aeson.TH       (deriveFromJSON)
import           Data.Aeson.Types    (typeMismatch)
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.Text           as T
import           Data.Time.Format    (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime (ZonedTime)
import           Universum

import           OrgStat.Scope       (AstPath (..), ScopeModifier (..))
import           OrgStat.Util        (dropLowerOptions)

data ConfDate = ConfNow | ConfZoned ZonedTime deriving (Show)

data ConfRange = ConfFromTo ConfDate ConfDate
               | ConfBlockWeek Int
               | ConfBlockDay Int
               | ConfBlockMonth Int
               deriving (Show)

data ConfReportType = Timeline ConfRange deriving (Show)


data ConfScope = ConfScope
    { csName  :: Maybe Text -- default needs no name
    , csNaths :: NonEmpty FilePath
    } deriving (Show)

data ConfReport = ConfReport
    { crType       :: ConfReportType -- includes config
    , crReportName :: Text
    , crModifiers  :: ScopeModifier
    } deriving (Show)

data OrgStatConfig = OrgStatConfig
    { confScopes    :: [ConfScope]
    , confReports   :: [ConfReport]
    , confOutputDir :: Maybe FilePath -- default is "./orgstat"
    } deriving (Show)

instance FromJSON AstPath where
    parseJSON (String s)
        | null s = fail "AstPath FromJson: empty string"
        | otherwise = pure $ AstPath $ T.splitOn "/" s
    parseJSON invalid    = typeMismatch "AstPath" invalid

instance FromJSON ScopeModifier where
    parseJSON (Object v) = do
        v .: "type" >>= \case
            (String "prune") -> ModPruneSubtree <$> v .: "path" <*> v .: "depth" .!= 0
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

instance FromJSON ConfReportType where
    parseJSON (Object v) = do
        v .: "type" >>= \case
            (String "timeline") -> Timeline <$> v .: "range"
            other -> fail $ "Unsupported scope modifier type: " ++ show other
    parseJSON invalid    = typeMismatch "ConfReportType" invalid

deriveFromJSON dropLowerOptions ''ConfScope
deriveFromJSON dropLowerOptions ''ConfReport
deriveFromJSON dropLowerOptions ''OrgStatConfig
