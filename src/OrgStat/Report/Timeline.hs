{-# LANGUAGE TemplateHaskell #-}

-- | Timeline reporting. Prouces a svg with columns.

module OrgStat.Report.Timeline
       ( TimelineParams (..)
       , tpColorSalt
       , tpLegend
       , tpTopDay
       , tpColumnWidth
       , tpColumnHeight
       , tpBackground

       , processTimeline
       ) where

import           Control.Lens         (makeLenses, (^.))
import           Data.Colour.CIE      (luminance)
import           Data.Default         (Default (..))
import           Data.List            (lookup, nub)
import qualified Data.Text            as T
import           Data.Time            (Day, DiffTime, LocalTime (..), UTCTime (..),
                                       addUTCTime, defaultTimeLocale, formatTime,
                                       timeOfDayToTime)
import           Diagrams.Backend.SVG (B)
import qualified Diagrams.Prelude     as D
import qualified Prelude
import           Text.Printf          (printf)
import           Universum

import           OrgStat.Ast          (Clock (..), Org (..))
import           OrgStat.Report.Types (SVGImageReport (..))
import           OrgStat.Util         (hashColour)


----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

data TimelineParams = TimelineParams
    { _tpColorSalt    :: Int    -- ^ Salt added when getting color out of task name.
    , _tpLegend       :: Bool   -- ^ Include map legend?
    , _tpTopDay       :: Int    -- ^ How many items to include in top day (under column)
    , _tpColumnWidth  :: Double -- ^ Coeff
    , _tpColumnHeight :: Double -- ^ Coeff
    , _tpBackground   :: (Word8,Word8,Word8)
    } deriving (Show)

instance Default TimelineParams where
    def = TimelineParams 0 True 5 1 1 (0xf2, 0xf2, 0xf2)

makeLenses ''TimelineParams

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
        Clock (LocalTime dFrom tFrom) (LocalTime dTo tTo) <- clocks
        guard $ any (== day) [dFrom, dTo]
        let tFrom' = if dFrom == day then timeOfDayToTime tFrom else fromInteger 0
        let tTo'   = if dTo   == day then timeOfDayToTime tTo   else fromInteger (24*60*60)
        pure (tFrom', tTo')

-- total time for each task
totalTimes :: [(Text, [(DiffTime, DiffTime)])] -> [(Text, DiffTime)]
totalTimes tasks = map (second clocksSum) tasks
  where
    clocksSum :: [(DiffTime, DiffTime)] -> DiffTime
    clocksSum clocks = sum $ map (\(start, end) -> end - start) clocks

-- list of leaves
orgToList :: Org -> [(Text, [Clock])]
orgToList = orgToList' ""
  where
    orgToList' :: Text -> Org -> [(Text, [Clock])]
    orgToList' _pr org =
      --let path = pr <> "/" <> _orgTitle org
      let path = _orgTitle org
      in (path, _orgClocks org) : concatMap (orgToList' path) (_orgSubtrees org)


----------------------------------------------------------------------------
-- Drawing
----------------------------------------------------------------------------


diffTimeSeconds :: DiffTime -> Integer
diffTimeSeconds time = floor $ toRational time

diffTimeMinutes :: DiffTime -> Integer
diffTimeMinutes time = diffTimeSeconds time `div` 60

-- diffTimeHours :: DiffTime -> Integer
-- diffTimeHours time = diffTimeMinutes time `div` 60


sRGB24Tuple :: (Floating a, Ord a) => (Word8,Word8,Word8) -> D.Colour a
sRGB24Tuple (a,b,c) = D.sRGB24 a b c

labelColour :: TimelineParams -> Text -> D.Colour Double
labelColour params _label = sRGB24Tuple $ hashColour (params ^. tpColorSalt) _label

-- | Returns if the label is to be shown. Second param is font-related
-- heuristic constant, third is length of interval.
fitLabelHeight :: TimelineParams -> Double -> Double -> Bool
fitLabelHeight params n h = h >= (params ^. tpColumnHeight) * n

-- | Decides by <heuristic param n depending on font>, width of column
-- and string, should it be truncated. And returns modified string.
fitLabelWidth :: TimelineParams -> Double -> Text -> Text
fitLabelWidth params n s =
    if T.length s <= toTake then s else T.take toTake s <> ".."
  where
    toTake = floor $ n * ((params ^. tpColumnWidth) ** 1.2)

-- timeline for a single day
timelineDay :: TimelineParams -> Day -> [(Text, (DiffTime, DiffTime))] -> D.Diagram B
timelineDay params day clocks =
    (D.strutY 5 D.===) $
    (dateLabel D.===) $
    D.scaleUToY height $
    (timeticks D.|||) $
    mconcat
      [ mconcat (map showClock clocks)
      , background
      ]
  where
    width = 140 * (totalHeight / height) * (params ^. tpColumnWidth)
    ticksWidth = 20 * (totalHeight / height)
    height = 700 * (params ^. tpColumnHeight)

    totalHeight :: Double
    totalHeight = 24*60

    timeticks :: D.Diagram B
    timeticks =
      mconcat $
      foreach [(0::Int)..23] $ \hour ->
      mconcat
        [ D.alignedText 0.5 1 (show hour)
          & D.font "DejaVu Sans"
          & D.fontSize 8
          & D.moveTo (D.p2 (0, -5))
        , D.rect ticksWidth 1
          & D.lw D.none
        ]
      & D.fc (D.sRGB24 150 150 150)
      & D.moveTo (D.p2 (0, totalHeight - fromIntegral hour * 60))

    dateLabel :: D.Diagram B
    dateLabel =
      mconcat
      [ D.strutY 20
      , D.alignedText 0 0.65 (formatTime defaultTimeLocale "%a, %d.%m.%Y" day)
        & D.font "DejaVu Sans"
        & D.fontSize 12
        & D.moveTo (D.p2 (25, 0))
      ]

    background :: D.Diagram B
    background =
      D.rect width totalHeight
      & D.lw D.none
      & D.fc (sRGB24Tuple $ params ^. tpBackground)
      & D.moveOriginTo (D.p2 (-width/2, totalHeight/2))
      & D.moveTo (D.p2 (0, totalHeight))

    contrastFrom c = if luminance c < 0.1 then D.sRGB24 224 224 224 else D.black

    showClock :: (Text, (DiffTime, DiffTime)) -> D.Diagram B
    showClock (label, (start, end)) =
      let
        w = width
        h = fromInteger $ diffTimeMinutes $ end - start
        bgboxColour = labelColour params label
        bgbox = D.rect w h
                & D.lw D.none
                & D.fc bgboxColour
        label' = D.alignedText 0 0.5 (T.unpack $ fitLabelWidth params 21 label)
               & D.font "DejaVu Sans"
               & D.fontSize 10
               & D.fc (contrastFrom bgboxColour)
               & D.moveTo (D.p2 (-w/2+10, 0))
        box = mconcat $ bool [] [label'] (fitLabelHeight params 14 h) ++ [bgbox]
      in box & D.moveOriginTo (D.p2 (-w/2, h/2))
             & D.moveTo (D.p2 (0, totalHeight - fromInteger (diffTimeMinutes start)))
-- timelines for several days, with top lists
timelineDays
  :: TimelineParams
  -> [Day]
  -> [[(Text, (DiffTime, DiffTime))]]
  -> [[(Text, DiffTime)]]
  -> D.Diagram B
timelineDays params days clocks topLists =
    D.hcat $
    foreach (days `zip` (clocks `zip` topLists)) $ \(day, (dayClocks, topList)) ->
      D.vsep 5
      [ timelineDay params day dayClocks
      , taskList params topList True
      ]

-- task list, with durations and colours
taskList :: TimelineParams -> [(Text, DiffTime)] -> Bool -> D.Diagram B
taskList params labels fit = D.vsep 5 $ map oneTask $ reverse $ sortOn snd labels
  where
    oneTask :: (Text, DiffTime) -> D.Diagram B
    oneTask (label, time) =
      D.hsep 3
      [ D.alignedText 1 0.5 (showTime time)
        & D.font "DejaVu Sans"
        & D.fontSize 10
        & D.translateX 30
      , D.rect 12 12
        & D.fc (labelColour params label)
        & D.lw D.none
      , D.alignedText 0 0.5 (T.unpack $ bool label (fitLabelWidth params 18 label) fit)
        & D.font "DejaVu Sans"
        & D.fontSize 10
      ]

    showTime :: DiffTime -> Prelude.String
    showTime time = printf "%d:%02d" hours minutes
      where
        (hours, minutes) = diffTimeMinutes time `divMod` 60

timelineReport :: TimelineParams -> Org -> (UTCTime, UTCTime) -> SVGImageReport
timelineReport params org (from,to) = SVGImage pic
  where
    lookupDef :: Eq a => b -> a -> [(a, b)] -> b
    lookupDef d a xs = fromMaybe d $ lookup a xs

    -- period to show. Right border is -1min, we assume it's non-inclusive
    daysToShow = [utctDay from .. utctDay ((negate 1) `addUTCTime` to)]

    -- unfiltered leaves
    tasks :: [(Text, [Clock])]
    tasks = orgToList org

    -- tasks from the given period, split by days
    byDay :: [[(Text, [(DiffTime, DiffTime)])]]
    byDay = selectDays daysToShow tasks

    -- total durations for each task, split by days
    byDayDurations :: [[(Text, DiffTime)]]
    byDayDurations = map totalTimes byDay

    -- total durations for the whole period
    allDaysDurations :: [(Text, DiffTime)]
    allDaysDurations =
      let allTasks = nub $ map fst $ concat byDayDurations in
      foreach allTasks $ \task ->
      (task,) $ sum $ foreach byDayDurations $ \durations ->
      lookupDef (fromInteger 0) task durations

    -- split clocks
    clocks :: [[(Text, (DiffTime, DiffTime))]]
    clocks = map allClocks byDay

    -- top list for each day
    topLists :: [[(Text, DiffTime)]]
    topLists =
        map (take (params ^. tpTopDay) . reverse . sortOn (\(_task, time) -> time))
        byDayDurations

    optLegend | params ^. tpLegend = [taskList params allDaysDurations False]
              | otherwise = []

    pic =
      D.vsep 30 $ [ timelineDays params daysToShow clocks topLists ] ++ optLegend

processTimeline
    :: (MonadThrow m)
    => TimelineParams -> Org -> (UTCTime, UTCTime) -> m SVGImageReport
processTimeline params org fromto = pure $ timelineReport params org fromto
