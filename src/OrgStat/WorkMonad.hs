{-# LANGUAGE TemplateHaskell #-}

-- | Definition for main work scope

module OrgStat.WorkMonad
       ( WorkScope (..)
       , wConfigFile
       , wXdgOpen
       , WorkM (..)
       , runWorkM
       ) where

import           Control.Lens (makeLenses)
import qualified System.Wlog  as W
import           Universum

data WorkScope = WorkScope
    { _wConfigFile :: FilePath
    , _wXdgOpen    :: Bool
    }

makeLenses ''WorkScope

newtype WorkM a = WorkM
    { getWorkM :: ReaderT WorkScope IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader WorkScope,
                W.CanLog, MonadThrow, MonadCatch)

instance W.HasLoggerName WorkM where
    getLoggerName = pure $ W.LoggerName "OrgStat"
    modifyLoggerName _ = identity

runWorkM :: MonadIO m => WorkScope -> WorkM a -> m a
runWorkM scope action = liftIO $ runReaderT (getWorkM action) scope
