{-# LANGUAGE TemplateHaskell #-}

-- | Definition for main work scope

module OrgStat.WorkMonad
       ( WorkConfig (..)
       , wcConfig
       , wcCommonArgs
       , WorkData
       , wdReadFiles
       , wdResolvedScopes
       , wdResolvedReports
       , WorkM (..)
       , runWorkM
       ) where

import Universum

import Control.Lens (makeLenses)
import Data.Default (Default (def))
import qualified System.Wlog as W

import OrgStat.Ast (Org)
import OrgStat.CLI (CommonArgs)
import OrgStat.Config (OrgStatConfig)

-- | Read-only app configuration.
data WorkConfig = WorkConfig
    { _wcConfig     :: OrgStatConfig
    , _wcCommonArgs :: CommonArgs
    }

makeLenses ''WorkConfig

-- | State component of application.
data WorkData = WorkData
    { _wdReadFiles       :: HashMap FilePath (Text, Org)
      -- ^ Org files that were read. Elements are pairs of type (file
      -- basename, content). Keys are paths.
    , _wdResolvedScopes  :: HashMap Text Org
      -- ^ Scope is just plain read files.
    , _wdResolvedReports :: HashMap Text Org
      -- ^ Report is a filtered (with scope modifiers and clock
      -- limitations) scope.
    }

makeLenses ''WorkData

instance Default WorkData where
    def = WorkData mempty mempty mempty

newtype WorkM a = WorkM
    { getWorkM :: StateT WorkData (ReaderT WorkConfig IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader WorkConfig
               , MonadState WorkData
               , W.CanLog
               , MonadThrow
               , MonadCatch
               )

instance W.HasLoggerName WorkM where
    askLoggerName = pure $ W.LoggerName "OrgStat"
    modifyLoggerName _ = identity

runWorkM :: MonadIO m => WorkConfig -> WorkM a -> m a
runWorkM config action =
    liftIO $ runReaderT (evalStateT (getWorkM action) def) config
