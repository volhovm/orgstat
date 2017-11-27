-- | Common args and parser.

module OrgStat.CLI
       ( CommonArgs (..)
       , parseCommonArgs
       ) where

import Universum

import Options.Applicative.Simple (Parser, help, long, metavar, strOption, switch)

-- | Read-only arguments that inner application needs (in contrast to,
-- say, logging severity).
data CommonArgs = CommonArgs
    { xdgOpen      :: !Bool
      -- ^ Open report types using xdg-open
    , selectOutput :: !(Maybe Text)
      -- ^ Single output can be selected instead of running all of them.
    , outputDir    :: !(Maybe FilePath)
      -- ^ Output directory for all ... outputs.
    } deriving Show

parseCommonArgs :: Parser CommonArgs
parseCommonArgs =
    CommonArgs <$>
    switch (long "xdg-open" <> help "Open each report using xdg-open") <*>
    optional (
        fromString <$>
        strOption (long "select-output" <>
                   help ("Output name you want to process " <>
                         "(by default all outputs from conf are processed"))) <*>
    optional (
        strOption (long "output-dir" <>
                   metavar "FILEPATH" <>
                   help ("Final output directory that overrides one in config. " <>
                         "No extra subdirectories will be created!")))
