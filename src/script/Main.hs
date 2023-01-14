{-# LANGUAGE ApplicativeDo, ScopedTypeVariables #-}

module Main where

import Universum

import Data.Default (def)
import Data.Version (showVersion)
import Options.Applicative.Simple
  (Parser, help, long, metavar, simpleOptions, strOption, switch, value)
import Paths_orgstat (version)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import OrgStat.CLI
import OrgStat.Config
import OrgStat.IO
import OrgStat.Logging
import OrgStat.Logic
import OrgStat.WorkMonad


config :: OrgStatConfig
config = OrgStatConfig
    { confScopes = []
    , confReports = []
    , confOutputs = []
    , confTimelineParams = undefined
    , confTodoKeywords = []
    , confOutputDir = "/home/volhovm/"
    }

commonArgs :: CommonArgs
commonArgs = def

main :: IO ()
main = do
    setLoggingSeverity Debug
    runOrgStatDefault config commonArgs
