module Main where

import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, help, long, metavar, simpleOptions,
                                             strOption, switch, value)
import           Universum

import           Paths_orgstat              (version)

data Args = Args
    { configPath :: !FilePath
      -- ^ Path to configuration file.
    , debug      :: Bool
      -- ^ Enable debug logging
    } deriving Show

argsParser :: Parser Args
argsParser =
    Args <$>
    strOption
        (long "conf-path" <> metavar "FILEPATH" <> value "~/.orgstat.conf" <>
        help "Path to the configuration file") <*>
    switch
        (long "debug" <>
         help
             "Enable debug logging")

getNodeOptions :: IO Args
getNodeOptions = do
    (res, ()) <-
        simpleOptions
            ("orgstat-" <> showVersion version)
            "----- OrgStat ------"
            "Statistic reports visualizer for org-mode"
            argsParser
            empty
    pure res

main :: IO ()
main = do
    args <- getNodeOptions
    when (debug args) $ putText $ "Was launched with options: " <> show args
