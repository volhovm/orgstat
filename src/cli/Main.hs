{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Catch        (catch)
import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, help, long, metavar, simpleOptions,
                                             strOption, switch, value)
import           Paths_orgstat              (version)
import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))
import           System.Wlog                (LoggingFormat (..), logDebug, logError)
import qualified System.Wlog                as W
import           Universum

import           OrgStat.Logic              (runOrgStat)
import           OrgStat.WorkMonad          (WorkScope (..), runWorkM)

data Args = Args
    { configPath :: !FilePath
      -- ^ Path to configuration file.
    , xdgOpen    :: Bool
      -- ^ Open report types using xdg-open
    , debug      :: Bool
      -- ^ Enable debug logging
    } deriving Show

argsParser :: FilePath -> Parser Args
argsParser homeDir =
    Args <$>
    strOption
        (long "conf-path" <> metavar "FILEPATH" <> value (homeDir </> ".orgstat.yaml") <>
        help "Path to the configuration file") <*>
    switch (long "xdg-open" <> help "Open each report using xdg-open") <*>
    switch (long "debug" <> help "Enable debug logging")

getNodeOptions :: FilePath -> IO Args
getNodeOptions homeDir = do
    (res, ()) <-
        simpleOptions
            ("orgstat-" <> showVersion version)
            "----- OrgStat ------"
            "Statistic reports visualizer for org-mode"
            (argsParser homeDir)
            empty
    pure res

main :: IO ()
main = do
    args@Args{..} <- getNodeOptions =<< getHomeDirectory
    W.initLoggingWith (LoggingFormat False) (if debug then W.Debug else W.Info)
    runWorkM (WorkScope configPath xdgOpen) $ do
        logDebug $ "Just started with options: " <> show args
        runOrgStat `catch` topHandler
  where
    topHandler (e :: SomeException) = do
        logError $ "Top level error occured: " <> show e
