{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Configuration file types, together with json instances.

module OrgStat.Config
       ( ConfigException (..)
       , ConfDate (..)
       , ConfRange (..)
       , ConfOutputType (..)
       , ConfOutput (..)
       , ConfScope (..)
       , ConfReport (..)
       , OrgStatConfig (..)
       ) where

import           Data.Aeson            (FromJSON (..), Value (Object, String), withObject,
                                        withText, (.!=), (.:), (.:?))
import           Data.Aeson.Types      (typeMismatch)
import           Data.Default          (def)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.Text             as T
import           Data.Time             (LocalTime)
import           Data.Time.Format      (defaultTimeLocale, parseTimeM)
import           Universum

import           OrgStat.Outputs.Types (SummaryParams (..), TimelineParams, tpBackground,
                                        tpColumnHeight, tpColumnWidth, tpLegend, tpTopDay)
import           OrgStat.Scope         (AstPath (..), ScopeModifier (..))
import           OrgStat.Util          (parseColour, (??~))

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

data ConfOutputType
    = TimelineOutput { toParams :: !TimelineParams
                     , toReport :: !Text }
    | SummaryOutput !SummaryParams
    deriving (Show)

data ConfScope = ConfScope
    { csName  :: !Text              -- default is "default"
    , csPaths :: !(NonEmpty FilePath)
    } deriving (Show)

data ConfOutput = ConfOutput
    { coType :: !ConfOutputType
    , coName :: !Text
    } deriving (Show)

data ConfReport = ConfReport
    { crName      :: !Text
    , crScope     :: !Text
    , crRange     :: !ConfRange
    , crModifiers :: ![ScopeModifier]
    } deriving (Show)

data OrgStatConfig = OrgStatConfig
    { confScopes             :: ![ConfScope]
    , confReports            :: ![ConfReport]
    , confOutputs            :: ![ConfOutput]
    , confBaseTimelineParams :: !TimelineParams
    , confTodoKeywords       :: ![Text]
    , confOutputDir          :: !FilePath -- default is "./orgstat"
    , confColorSalt          :: !Int
    } deriving (Show)

instance FromJSON AstPath where
    parseJSON = withText "AstPath" $ \s ->
        pure $ AstPath $ filter (not . T.null) $ T.splitOn "/" s

instance FromJSON ScopeModifier where
    parseJSON  = withObject "ScopeModifier" $ \o -> do
        o .: "type" >>= \case
            (String "prune") -> ModPruneSubtree <$> o .: "path" <*> o .:? "depth" .!= 0
            (String "select") -> ModSelectSubtree <$> o .: "path"
            (String "filterbytag") -> ModFilterTag <$> o .: "tag"
            other -> fail $ "Unsupported scope modifier type: " ++ show other

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
    parseJSON = withObject "TimelineParams" $ \v -> do
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

instance FromJSON ConfOutputType where
    parseJSON = withObject "ConfOutputType" $ \o ->
        o .: "type" >>= \case
            (String "timeline") -> do
                toReport <- o .: "report"
                toParams <- parseJSON (Object o)
                pure $ TimelineOutput {..}
            (String "summary") -> do
                soTemplate <- o .: "template"
                pure $ SummaryOutput $ SummaryParams soTemplate
            other -> fail $ "Unsupported output type: " ++ show other

instance FromJSON ConfOutput where
    parseJSON = withObject "ConfOutput" $ \o -> do
        coName   <- o .: "name"
        coType  <- parseJSON (Object o)
        pure $ ConfOutput{..}

instance FromJSON ConfScope where
    parseJSON = withObject "ConfScope" $ \o ->
        ConfScope <$> o .:? "name" .!= "default"
                  <*> o .: "paths"

instance FromJSON ConfReport where
    parseJSON = withObject "ConfReport" $ \o ->
        ConfReport <$> o .: "name"
                   <*> o .:? "scope" .!= "default"
                   <*> o .: "range"
                   <*> o .:? "modifiers" .!= []

instance FromJSON OrgStatConfig where
    parseJSON = withObject "OrgStatConfig" $ \o ->
        OrgStatConfig <$> o .: "scopes"
                      <*> o .: "reports"
                      <*> o .: "outputs"
                      <*> o .:? "timelineDefault" .!= def
                      <*> o .:? "todoKeywords" .!= []
                      <*> (o .:? "outputDir" <|> o .:? "output") .!= "./orgstat"
                      <*> o .:? "colorSalt" .!= 0
