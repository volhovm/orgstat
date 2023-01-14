-- | Common args and parser.

module OrgStat.CLI
       ( CommonArgs (..)
       , parseCommonArgs
       ) where

import Universum

import Data.Default (Default(..))
import Options.Applicative.Simple (Parser, help, long, metavar, strOption)

-- | Read-only arguments that inner application needs (in contrast to,
-- say, logging severity).
data CommonArgs = CommonArgs
    {
      caOutputs   :: !(Maybe [Text])
      -- ^ Selected outputs to run; Nothing is interpreted as generate
      -- all available outputs.
    , caOutputDir :: !(Maybe FilePath)
      -- ^ Output directory for all ... outputs.
    } deriving Show

instance Default CommonArgs where
    def = CommonArgs Nothing Nothing

parseCommonArgs :: Parser CommonArgs
parseCommonArgs =
    CommonArgs <$>
    optional ( -- TODO test this parsing works correctly
    many (
        fromString <$>
        strOption (long "output" <>
                   long "select-output" <>
                   help ("Output name(s) you want to process " <>
                         "(default: all outputs are processed)")))) <*>
    optional (
        strOption (long "output-dir" <>
                   metavar "FILEPATH" <>
                   help ("Final output directory that overrides one in config. " <>
                         "No extra subdirectories will be created")))
