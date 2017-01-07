-- | Timeline reporting. Prouces a svg with columns.

module OrgStat.Report.Timeline
       ( TimelineParams (..)
       , processTimeline
       ) where

import qualified Data.Attoparsec.Text as A
import           Data.Default         (Default (..))
import qualified Data.Text            as T
import           Data.Time            (Day, DiffTime, UTCTime (..), fromGregorian)
import           Diagrams.Backend.SVG (B)
import qualified Diagrams.Backend.SVG as DB
import qualified Diagrams.Prelude     as D
import           OrgStat.Parser       (parseOrg)
import           Universum

import           OrgStat.Ast          (Clock (..), Org (..))
import           OrgStat.Report.Types (SVGImageReport (..))

data TimelineParams = TimelineParams
    { _tpColorSalt   :: Int    -- ^ Salt added when getting color out of task name.
    , _tpLegend      :: Bool   -- ^ Include map legend?
    , _tpTopDay      :: Int    -- ^ How many items to include in top day (under column)
    , _tpColumnWidth :: Double -- ^ Coeff
    } deriving Show

instance Default TimelineParams where
    def = TimelineParams 0 True 5 1

diffTimeToMinutes :: DiffTime -> Double
diffTimeToMinutes time = fromInteger $ floor $ (/ 60) $ toRational time

-- timeline for a single day
timelineDay :: [(Text, (DiffTime, DiffTime))] -> D.Diagram B
timelineDay clocks = mconcat
    [ mconcat (map showClock clocks)
    , background
    ]
  where
    width = 250
    totalHeight = 24*60

    colour :: Text -> D.Colour Double
    colour _ = D.pink

    background :: D.Diagram B
    background =
      D.rect width totalHeight
      & D.lw D.none
      & D.fc D.red
      & D.moveOriginTo (D.p2 (-width/2, totalHeight/2))
      & D.moveTo (D.p2 (0, totalHeight))

    showClock :: (Text, (DiffTime, DiffTime)) -> D.Diagram B
    showClock (label, (start, end)) =
      let
        w = width
        h = diffTimeToMinutes $ end - start
      in
        mconcat
          [ D.alignedText 0 0.5 (T.unpack label)
            & D.font "DejaVu Sans" & D.fontSize 10
            & D.moveTo (D.p2 (-w/2+10, 0))
          , D.rect w h
            & D.lw (D.output 0.5)
            & D.fc (colour label)
          ]
        & D.moveOriginTo (D.p2 (-w/2, h/2))
        & D.moveTo (D.p2 (0, totalHeight - diffTimeToMinutes start))

-- timelines for several days
timelineDays :: [[(Text, (DiffTime, DiffTime))]] -> D.Diagram B
timelineDays times = D.hsep 10 $ map timelineDay times

processTimeline :: (MonadThrow m) => TimelineParams -> Org -> m SVGImageReport
processTimeline = notImplemented
