{-# LANGUAGE ApplicativeDo, ScopedTypeVariables #-}

module Main where

import Universum

import qualified Data.Text as T
import Data.Version (showVersion)
import Options.Applicative.Simple
  (Parser, help, long, metavar, simpleOptions, strOption, switch, value)
import Paths_orgstat (version)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Turtle (shell)

import OrgStat.CLI (CommonArgs, parseCommonArgs)
import OrgStat.IO (readConfig)
import OrgStat.Logging (Severity(..), logDebug, logError, logInfo, setLoggingSeverity)
import OrgStat.Logic (runOrgStat)
import OrgStat.WorkMonad (WorkConfig(..), runWorkM)


data Args = Args
    { configPath :: !FilePath
      -- ^ Path to configuration file.
    , debug      :: !Bool
      -- ^ Enable debug logging.
    , xdgOpen    :: !Bool
      -- ^ Open report types using xdg-open
    , commonArgs :: CommonArgs
      -- ^ Other arguments.
    } deriving Show

argsParser :: FilePath -> Parser Args
argsParser homeDir = do
    configPath <-
        strOption
            (long "conf-path" <> metavar "FILEPATH" <> value (homeDir </> ".orgstat.yaml") <>
            help "Path to the configuration file")
    debug <- switch (long "debug" <> help "Enable debug logging")
    xdgOpen <- switch (long "xdg-open" <> help "Open each report using xdg-open")
    commonArgs <- parseCommonArgs
    pure Args {..}

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
    setLoggingSeverity $ if debug then Debug else Info
    config <- readConfig configPath
    runWorkM (WorkConfig config commonArgs) $ do
        logDebug $ "Just started with options: " <> show args
        runOrgStat `catch` topHandler

    when xdgOpen $ do
        reportDir::String <- undefined -- getOutputDir
        logInfo "Opening reports using xdg-open..."
        void $ shell ("for i in $(ls "<>T.pack reportDir<>"/*); do xdg-open $i; done") empty

  where
    topHandler (e :: SomeException) = do
        logError $ "Top level error occured: " <> show e
