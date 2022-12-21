{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeApplications #-}
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

import Data.Aeson (FromJSON(..), Value(Object, String), withObject, withText, (.!=), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Data.Time (LocalTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Universum

import OrgStat.Ast (Tag(..), Title(..))
import OrgStat.Outputs.Types
  (BlockParams(..), ScriptParams(..), SummaryParams(..), TimelineParams(..))
import OrgStat.Scope (AstPath(..), ScopeModifier(..))
import OrgStat.Util (parseColour)

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
    | ConfRelDay !Integer
    | ConfRelWeek !Integer
    | ConfRelMonth !Integer
    deriving (Show)

data ConfRange
    = ConfFromTo !ConfDate !ConfDate
    deriving (Show)

data ConfOutputType
    = TimelineOutput { toParams :: !TimelineParams
                     , toReport :: !Text }
    | SummaryOutput !SummaryParams
    | ScriptOutput !ScriptParams
    | BlockOutput { boParams :: !BlockParams
                  , boReport :: !Text }
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
    { confScopes         :: ![ConfScope]
    , confReports        :: ![ConfReport]
    , confOutputs        :: ![ConfOutput]
    , confTimelineParams :: !TimelineParams
    , confTodoKeywords   :: ![Text]
    , confOutputDir      :: !FilePath -- default is "./orgstat"
    } deriving (Show)

instance FromJSON Tag where
    parseJSON = withText "Tag" $ pure . Tag

instance FromJSON AstPath where
    parseJSON = withText "AstPath" $ \s ->
        pure $ AstPath $ map Title $ filter (not . T.null) $ T.splitOn "/" s

instance FromJSON ScopeModifier where
    parseJSON  = withObject "ScopeModifier" $ \o -> do
        o .: "type" >>= \case
            (String "prune") -> ModPruneSubtree <$> o .: "path" <*> o .:? "depth" .!= 0
            (String "select") -> ModSelectSubtree <$> o .: "path"
            (String "filterbytag") ->
                (ModFilterTags . (\x -> [x]) <$> o .: "tag") <|>
                (ModFilterTags <$> o .: "tags")
            other -> fail $ "Unsupported scope modifier type: " ++ show other

instance FromJSON ConfDate where
    parseJSON (String "now") = pure $ ConfNow
    parseJSON (String s) | any (`T.isPrefixOf` s) ["day", "week", "month"] = do
        let splitted = T.splitOn "-" $ if "-" `T.isInfixOf` s then s else s <> "-"
            [range, number] = splitted
            constructor :: (Monad m, MonadFail m) => Integer -> m ConfDate
            constructor i = case range of
                "day"   -> pure $ ConfRelDay i
                "week"  -> pure $ ConfRelWeek i
                "month" -> pure $ ConfRelMonth i
                t       -> fail $ "ConfDate@parseJSON can't parse " <> T.unpack t <>
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
    parseJSON (String s)     =
        case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" (T.unpack s) of
            Nothing -> fail $
                "Couldn't read date " <> show s <>
                ". Correct format is 2016-01-01 23:59"
            Just ut -> pure $ ConfLocal ut
    parseJSON invalid        = typeMismatch "ConfDate" invalid

instance FromJSON ConfRange where
    parseJSON (Object v)       = ConfFromTo <$> v .: "from" <*> v .: "to"
    parseJSON (String s) | any (`T.isPrefixOf` s) ["day", "week", "month"] = do
        let splitted = T.splitOn "-" $ if "-" `T.isInfixOf` s then s else s <> "-"
            [range, number] = splitted
            constructor :: (Monad m, MonadFail m) => Integer -> m ConfRange
            constructor i = case range of
                "day"   -> pure $ ConfFromTo (ConfRelDay i) (ConfRelDay $ i-1)
                "week"  -> pure $ ConfFromTo (ConfRelWeek i) (ConfRelWeek $ i-1)
                "month" -> pure $ ConfFromTo (ConfRelMonth i) (ConfRelMonth $ i-1)
                t       -> fail $ "ConfDate@parseJSON can't parse " <> T.unpack t <>
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
    parseJSON invalid          = typeMismatch "ConfRange" invalid

instance FromJSON TimelineParams where
    parseJSON = withObject "TimelineParams" $ \v -> do
        _tpColorSalt <- v .:? "colorSalt" .!= 0
        _tpLegend <- v .:? "legend" .!= True
        _tpTopDay <- v .:? "topDay" .!= 5
        _tpColumnWidth <- v .:? "colWidth" .!= 1.0
        _tpLegendColumnWidth <- v.:? "legendColWidth" .!= 1.0
        _tpColumnHeight <- v .:? "colHeight" .!= 1.0
        _tpVSepWidth <- v .:? "vSepWidth" .!= 1.0
        _tpWeekStartsMonday <- v .:? "weekStartsMonday" .!= True
        _tpBackground <-
            (fromMaybe (error "Can't parse colour") . parseColour @Text . T.strip) <$>
            v .:? "background" .!= "#ffffff"
        pure TimelineParams{..}

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
            (String "script") -> do
                spScript <-
                    fmap Left (o .: "scriptPath") <|>
                    fmap Right (o .: "inline")
                spReports <- o .: "reports"
                spInterpreter <- o .:? "interpreter" .!= "sh"
                pure $ ScriptOutput $ ScriptParams spScript spReports spInterpreter
            (String "block") -> do
                boReport <- o .: "report"
                _bpMaxLength <- o .:? "maxLength" .!= 80
                _bpUnicode  <- o .:? "unicode" .!= True
                let boParams = BlockParams{..}
                pure $ BlockOutput {..}
            other -> fail $ "Unsupported output type: " ++ show other

instance FromJSON ConfOutput where
    parseJSON = withObject "ConfOutput" $ \o -> do
        coName <- o .: "name"
        coType <- parseJSON (Object o)
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
                      -- timelineDefault is deprecated
                      <*> ((o .: "timelineParams" <|> o .: "timelineDefault")
                           <|> parseJSON (Object mempty))
                      <*> o .:? "todoKeywords" .!= []
                      <*> (o .:? "outputDir" <|> o .:? "output") .!= "./orgstat"
