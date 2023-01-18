{-# LANGUAGE RankNTypes, TemplateHaskell #-}
-- | Types common among reports.

module OrgStat.Outputs.Types
       ( TimelineParams (..)
       , tpColorSalt
       , tpLegend
       , tpLegendColumnWidth
       , tpTopDay
       , tpColumnWidth
       , tpColumnHeight
       , tpVSepWidth
       , tpBackground
       , tpWeekStartsMonday
       , TimelineOutput (..)

       , SummaryParams (..)
       , SummaryOutput (..)

       , ScriptParams (..)

       , BlockParams (..)
       , bpMaxLength
       , bpUnicode
       , BlockOutput (..)
       ) where

import Universum

import Control.Lens (makeLenses)

import Data.Default (Default(..))
import Diagrams.Backend.SVG (B)
import qualified Diagrams.Prelude as D

----------------------------------------------------------------------------
-- Timeline
----------------------------------------------------------------------------

data TimelineParams = TimelineParams
    { _tpColorSalt    :: !Int
      -- ^ Salt added when getting color out of task name.
    , _tpLegend       :: !Bool
      -- ^ Include map legend?
    , _tpLegendColumnWidth       :: !Double
      -- ^ Legend column width in percent
    , _tpTopDay       :: !Int
      -- ^ How many items to include in top day (under column)
    , _tpColumnWidth  :: !Double
      -- ^ Column width in percent
    , _tpColumnHeight :: !Double
      -- ^ Column height
    , _tpWeekStartsMonday :: !Bool
      -- ^ Whether week starts on Monday (otherwise assumed Sunday)
    , _tpVSepWidth :: !Double
      -- ^ Width of the vertical separator columns (with hours)
    , _tpBackground   :: !String
      -- ^ Color of background, "#RRGGBB"
    } deriving (Show)

makeLenses ''TimelineParams


instance Default TimelineParams where
    def = TimelineParams
        { _tpColorSalt = 0
        , _tpLegend = True
        , _tpTopDay =  5
        , _tpColumnWidth =  1.0
        , _tpLegendColumnWidth =  1.0
        , _tpColumnHeight = 1.0
        , _tpVSepWidth = 1.0
        , _tpWeekStartsMonday = True
        , _tpBackground = "#ffffff"
        }


-- | SVG timeline image.
newtype TimelineOutput = TimelineOutput (D.Diagram B)

----------------------------------------------------------------------------
-- Summary
----------------------------------------------------------------------------

-- | Parameters of the summary output
data SummaryParams = SummaryParams
    { spTemplate :: !Text
      -- ^ Formatting template.
    } deriving Show

-- | Some text (supposed to be single line or something).
newtype SummaryOutput = SummaryOutput Text

----------------------------------------------------------------------------
-- Script
----------------------------------------------------------------------------

-- | Parameters of the summary output
data ScriptParams = ScriptParams
    { spScript      :: !(Either FilePath Text)
      -- ^ Either path to the script to execute, or a script text itself.
    , spReports     :: ![Text]
      -- ^ Reports to consider.
    , spInterpreter :: !String
      -- ^ Interpreter to use.
    } deriving Show

----------------------------------------------------------------------------
-- Block
----------------------------------------------------------------------------

-- | Parameters for block output. Stub (for now).
data BlockParams = BlockParams
    { _bpMaxLength :: !Int
      -- ^ Maximum title length (together with indentation).
    , _bpUnicode   :: !Bool
      -- ^ Should unicode symbols be used for box borders.
    } deriving (Show)

makeLenses ''BlockParams

instance Default BlockParams where
    def = BlockParams 80 True

-- | Output of block type is text file, basically.
newtype BlockOutput = BlockOutput Text
