-- | Timeline reporting. Prouces a svg with columns.

module OrgStat.Report.Timeline
       ( TimelineParams (..)
       , processTimeline
       ) where

import qualified Data.Attoparsec.Text as A
import           Data.Default         (Default (..))
import           Data.List            (nub)
import qualified Data.Text            as T
import           Data.Time            (Day, DiffTime, UTCTime (..), fromGregorian)
import           Diagrams.Backend.SVG (B)
import qualified Diagrams.Backend.SVG as DB
import qualified Diagrams.Prelude     as D
import           OrgStat.Parser       (parseOrg)
import           Universum

import           OrgStat.Ast          (Clock (..), Org (..))
import           OrgStat.Report.Types (SVGImageReport (..))


----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

data TimelineParams = TimelineParams
    { _tpColorSalt   :: Int    -- ^ Salt added when getting color out of task name.
    , _tpLegend      :: Bool   -- ^ Include map legend?
    , _tpTopDay      :: Int    -- ^ How many items to include in top day (under column)
    , _tpColumnWidth :: Double -- ^ Coeff
    } deriving Show

instance Default TimelineParams where
    def = TimelineParams 0 True 5 1


----------------------------------------------------------------------------
-- Processing clocks
----------------------------------------------------------------------------

-- [(a, [b])] -> [(a, b)]
allClocks :: [(Text, [(DiffTime, DiffTime)])] -> [(Text, (DiffTime, DiffTime))]
allClocks tasks = do
  (label, clocks) <- tasks
  clock <- clocks
  pure (label, clock)

-- separate list for each day
selectDays :: [Day] -> [(Text, [Clock])] -> [[(Text, [(DiffTime, DiffTime)])]]
selectDays days tasks =
    foreach days $ \day ->
      filter (not . null . snd) $
      map (second (selectDay day)) tasks
  where
    selectDay :: Day -> [Clock] -> [(DiffTime, DiffTime)]
    selectDay day clocks = do
        (Clock (UTCTime dFrom tFrom) (UTCTime dTo tTo)) <- clocks
        guard $ any (== day) [dFrom, dTo]
        let tFrom' = if dFrom == day then tFrom else fromInteger 0
        let tTo'   = if dTo   == day then tTo   else fromInteger (24*60*60)
        pure (tFrom', tTo')

-- list of leaves
orgToList :: Org -> [(Text, [Clock])]
orgToList = orgToList' ""
  where
    orgToList' :: Text -> Org -> [(Text, [Clock])]
    orgToList' _pr org =
      --let path = pr <> "/" <> _orgTitle org
      let path = _orgTitle org
      in case _orgSubtrees org of
        [] -> [(path, _orgClocks org)]
        _  -> concatMap (orgToList' path) (_orgSubtrees org)


----------------------------------------------------------------------------
-- Drawing
----------------------------------------------------------------------------

diffTimeToMinutes :: DiffTime -> Double
diffTimeToMinutes time = fromInteger $ floor $ (/ 60) $ toRational time

labelColour :: TimelineParams -> (Text -> D.Colour Double)
labelColour _params _label = D.pink

-- timeline for a single day
timelineDay :: TimelineParams -> [(Text, (DiffTime, DiffTime))] -> D.Diagram B
timelineDay params clocks =
  D.scaleUToY height $
  mconcat
    [ mconcat (map showClock clocks)
    , background
    ]
  where
    width = 140 * (totalHeight / height)
    height = 700

    totalHeight = 24*60

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
            & D.font "DejaVu Sans"
            & D.fontSize 10
            & D.moveTo (D.p2 (-w/2+10, 0))
          , D.rect w h
            & D.lw (D.output 0.5)
            & D.fc (labelColour params label)
          ]
        & D.moveOriginTo (D.p2 (-w/2, h/2))
        & D.moveTo (D.p2 (0, totalHeight - diffTimeToMinutes start))

-- timelines for several days
timelineDays :: TimelineParams -> [[(Text, (DiffTime, DiffTime))]] -> D.Diagram B
timelineDays params times = D.hsep 10 $ map (timelineDay params) times

-- tasks with their colours
taskList :: TimelineParams -> [Text] -> D.Diagram B
taskList params labels = D.vsep 5 $ map oneTask labels
  where
    oneTask :: Text -> D.Diagram B
    oneTask label =
      D.hsep 5
      [ D.rect 12 12
        & D.fc (labelColour params label)
        & D.lw D.none
      , D.alignedText 0 0.5 (T.unpack label)
        & D.font "DejaVu Sans" & D.fontSize 10
      ]

timelineReport :: TimelineParams -> Org -> SVGImageReport
timelineReport params org = SVGImage pic
  where
    daysToShow =
      foreach [1..7] $ \day ->
      fromGregorian 2017 1 day
    width = D.width pic
    height = D.height pic

    tasks = orgToList org
    byDay = selectDays daysToShow tasks
    clocks = map allClocks byDay

    pic =
      D.vsep 5
      [ timelineDays params clocks
      , taskList params (nub $ map fst $ concat byDay)
      ]

processTimeline :: (MonadThrow m) => TimelineParams -> Org -> m SVGImageReport
processTimeline params org = pure $ timelineReport params org

-- test
mm :: IO ()
mm = do
    txt <- readFile "/home/zhenya/Dropbox/org/proj.org"
    let Right org = A.parseOnly (parseOrg ["!","&","+"]) txt
    let SVGImage pic = timelineReport def org
    DB.renderSVG "./tmp/some.svg" (D.dims2D (D.width pic) (D.height pic)) pic
