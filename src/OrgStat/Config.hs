{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Configuration file parser, together with json instances.

module OrgStat.Config
       ( ConfReportType(..)
       , ConfScope (..)
       , ConfReport (..)
       , OrgStatConfig (..)
       ) where

import           Data.Aeson         (FromJSON (..), Value (String))
import           Data.Aeson.TH      (defaultOptions, deriveFromJSON, deriveJSON,
                                     deriveToJSON)
import           Data.Aeson.Types   (typeMismatch)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text          as T
import           Universum

import           OrgStat.Scope      (AstPath (..), ScopeModifier (..))

data ConfReportType = Timeline

data ConfScope = ConfScope
    { csName  :: Maybe Text -- default needs no name
    , csNaths :: NonEmpty FilePath
    }

data ConfReport = ConfReport
    { crType       :: ConfReportType -- includes config
    , crReportName :: Text
    , crModifiers  :: ScopeModifier
    }

data OrgStatConfig = OrgStatConfig
    { confScopes    :: [ConfScope]
    , confReports   :: [ConfReport]
    , confOutputDir :: Maybe FilePath -- default is "./orgstat"
    }

instance FromJSON AstPath where
    parseJSON (String s)
        | null s = fail "AstPath FromJson: empty string"
        | otherwise = pure $ AstPath $ T.splitOn "/" s
    parseJSON invalid    = typeMismatch "AstPath" invalid

deriveToJSON defaultOptions ''AstPath
deriveJSON defaultOptions ''ScopeModifier
deriveFromJSON defaultOptions ''ConfReportType
deriveFromJSON defaultOptions ''ConfScope
deriveFromJSON defaultOptions ''ConfReport
