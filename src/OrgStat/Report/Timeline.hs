-- | Timeline reporting. Prouces a svg with columns.

module OrgStat.Report.Timeline
       ( TimelineParams (..)
       , processTimeline
       ) where

import           Data.Default         (Default (..))
import           Universum

import           OrgStat.Ast          (Org)
import           OrgStat.Report.Types (SVGImageReport)

data TimelineParams = TimelineParams
    { _tpColorSalt   :: Int    -- ^ Salt added when getting color out of task name.
    , _tpLegend      :: Bool   -- ^ Include map legend?
    , _tpTopDay      :: Int    -- ^ How many items to include in top day (under column)
    , _tpColumnWidth :: Double -- ^ Coeff
    } deriving Show

instance Default TimelineParams where
    def = TimelineParams 0 True 5 1

processTimeline :: (MonadThrow m) => TimelineParams -> Org -> m SVGImageReport
processTimeline = notImplemented
