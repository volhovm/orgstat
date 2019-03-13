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
    { caXdgOpen   :: !Bool
      -- ^ Open report types using xdg-open
    , caOutputs   :: ![Text]
      -- ^ Single output can be selected instead of running all of them.
    , caOutputDir :: !(Maybe FilePath)
      -- ^ Output directory for all ... outputs.
    } deriving Show

parseCommonArgs :: Parser CommonArgs
parseCommonArgs =
    CommonArgs <$>
    switch (long "xdg-open" <> help "Open each report using xdg-open") <*>
    many (
        fromString <$>
        strOption (long "output" <>
                   long "select-output" <>
                   help ("Output name(s) you want to process " <>
                         "(default: all outputs are processed)"))) <*>
    optional (
        strOption (long "output-dir" <>
                   metavar "FILEPATH" <>
                   help ("Final output directory that overrides one in config. " <>
                         "No extra subdirectories will be created")))
