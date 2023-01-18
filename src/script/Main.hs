{-# LANGUAGE ApplicativeDo, OverloadedLists, ScopedTypeVariables #-}

module Main where

import Universum

import Data.Default (def)

import OrgStat.CLI
import OrgStat.Config
import OrgStat.Logging
import OrgStat.Logic


myScopes =
    [ ConfScope
      "defscope"
      ["/home/volhovm/org/study.org","/home/volhovm/org/private.org"]
    ]

myReports =
    [ ConfReport "thisWeekAll" "defscope" (ConfFromTo (ConfRelWeek 1) (ConfRelWeek 0)) []
    ]

myOutputs =
    [ ConfOutput (TimelineOutput def "thisWeekAll") "thisWeekAllTimeline"
    ]

config :: OrgStatConfig
config = OrgStatConfig
    { confScopes = myScopes
    , confReports = myReports
    , confOutputs = myOutputs
    , confTimelineParams = def
    , confTodoKeywords = ["TD", "ST", "WT", "CL", "DN"]
    , confOutputDir = "/home/volhovm/"
    }

commonArgs :: CommonArgs
commonArgs = def

main :: IO ()
main = do
    setLoggingSeverity Debug
    runOrgStatDefault config commonArgs
