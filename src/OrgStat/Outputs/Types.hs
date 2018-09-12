{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Types common among reports.

module OrgStat.Outputs.Types
       ( TimelineParams (..)
       , tpColorSalt
       , tpLegend
       , tpTopDay
       , tpColumnWidth
       , tpColumnHeight
       , tpBackground
       , TimelineOutput (..)

       , SummaryParams (..)
       , SummaryOutput (..)

       , BlockParams (..)
       , bpMaxLength
       , bpUnicode
       , BlockOutput (..)
       ) where

import Universum

import Control.Lens (makeLenses)

import GHC.Generics as GHC
import Data.Default (Default (..))
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
    , _tpTopDay       :: !Int
      -- ^ How many items to include in top day (under column)
    , _tpColumnWidth  :: !Double
      -- ^ Column width in percent
    , _tpColumnHeight :: !Double
      -- ^ Column height
    , _tpBackground   :: !(D.Colour Double)
      -- ^ Color of background
    } deriving (Show,GHC.Generic)

instance Semigroup TimelineParams

instance Default TimelineParams where
    def = TimelineParams 0 True 5 1 1 (D.sRGB24 0xf2 0xf2 0xf2)

makeLenses ''TimelineParams

-- | For all non-default field values of RHS, override LHS with them.
mergeParams :: TimelineParams -> TimelineParams -> TimelineParams
mergeParams lhs rhs = mods lhs
  where
    mods = foldr1 (.)
           [ asId tpColorSalt
           , asId tpLegend
           , asId tpTopDay
           , asId tpColumnWidth
           , asId tpColumnHeight
           , asId tpBackground ]
    asId :: forall b. (Eq b) => Lens' TimelineParams b -> TimelineParams -> TimelineParams
    asId l x =
        if def ^. l == rhs ^. l
        then x else x & l .~ (rhs ^. l)

instance Monoid TimelineParams where
    mempty = def
    mappend = mergeParams

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
-- Block
----------------------------------------------------------------------------

-- | Parameters for block output. Stub (for now).
data BlockParams = BlockParams
    { _bpMaxLength :: Int
      -- ^ Maximum title length (together with indentation).
    , _bpUnicode   :: Bool
    } deriving (Show)

makeLenses ''BlockParams

instance Default BlockParams where
    def = BlockParams 80 True

-- | Output of block type is text file, basically.
newtype BlockOutput = BlockOutput Text
