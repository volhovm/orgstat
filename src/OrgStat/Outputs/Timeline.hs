{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell #-}

-- | Timeline reporting output. Prouces a svg with columns.

module OrgStat.Outputs.Timeline
       ( processTimeline
       ) where

import Data.Colour.CIE (luminance)
import Data.List (lookup, nub)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
  (Day, DayOfWeek(..), DiffTime, LocalTime(..), dayOfWeek, defaultTimeLocale, formatTime,
  timeOfDayToTime)
import Diagrams.Backend.SVG (B)
import qualified Diagrams.Prelude as D
import qualified Prelude
import Text.Printf (printf)
import Universum

import OrgStat.Ast (Clock(..), Org(..), Tag(..), Title(..), orgClocks, orgTags, traverseTree)
import OrgStat.Outputs.Types
  (TimelineOutput(..), TimelineParams, tpBackground, tpColorSalt, tpColumnHeight, tpColumnWidth,
  tpLegend, tpLegendColumnWidth, tpTopDay, tpVSepWidth, tpWeekStartsMonday)
import OrgStat.Util (addLocalTime, hashColour, parseColour)



----------------------------------------------------------------------------
-- Processing clocks
----------------------------------------------------------------------------

-- [(a, [b])] -> [(a, b)]
allClocks :: [(Org, [(DiffTime, DiffTime)])] -> [(Org, (DiffTime, DiffTime))]
allClocks tasks = do
    (label, clocks) <- tasks
    clock <- clocks
    pure (label, clock)

-- separate list for each day
selectDays :: [Day] -> [(Org, [Clock])] -> [[(Org, [(DiffTime, DiffTime)])]]
selectDays days tasks =
    flip map days $ \day ->
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
totalTimes :: [(Org, [(DiffTime, DiffTime)])] -> [(Org, DiffTime)]
totalTimes tasks = map (second clocksSum) tasks
  where
    clocksSum :: [(DiffTime, DiffTime)] -> DiffTime
    clocksSum clocks = sum $ map (\(start, end) -> end - start) clocks

-- list of nodes and their clocks, with tags propagated
orgFlatten :: Org -> [(Org, [Clock])]
orgFlatten = orgFlatten' []
  where
    orgFlatten' :: [Tag] -> Org -> [(Org, [Clock])]
    orgFlatten' tags org =
        let curTags = org ^. orgTags in
        let newTags = ordNub $ curTags <> tags in
        (org & orgTags .~ newTags, _orgClocks org) :
        concatMap (orgFlatten' newTags) (_orgSubtrees org)


----------------------------------------------------------------------------
-- Drawing
----------------------------------------------------------------------------


diffTimeSeconds :: DiffTime -> Integer
diffTimeSeconds time = floor $ toRational time

diffTimeMinutes :: DiffTime -> Integer
diffTimeMinutes time = diffTimeSeconds time `div` 60

-- diffTimeHours :: DiffTime -> Integer
-- diffTimeHours time = diffTimeMinutes time `div` 60


entryColour :: TimelineParams -> Org -> D.Colour Double
entryColour params org = case tags of
    [] -> hashColour (params ^. tpColorSalt) (getTitle $ _orgTitle org)
    xs@(x:_) ->
        -- trace (show (_orgTitle org) <> ": " <> show (_orgTags org)) $
        if "A" `elem` xs then convert "#AAAAAA"
        else if "M" `elem` xs then convert "#0000FF"
        else if "H" `elem` xs then convert "#00FFFF"
        else if "E" `elem` xs then convert "#FFFF00"
        else if "sleep" `elem` xs then convert "#DDA0DD"
        else hashColour (params ^. tpColorSalt) x
  where
    convert (s :: String) =
        fromMaybe (error "entryColour: cannot convert") $ parseColour s
    tags = map getTag $ _orgTags org

-- | Returns if the label is to be shown. Second param is font-related
-- heuristic constant, third is length of interval.
fitLabelHeight :: TimelineParams -> Double -> Double -> Bool
fitLabelHeight params n h = h >= (params ^. tpColumnHeight) * n

-- TODO improve! this is very vague.
-- | Truncates the string, heurestically, to fit into a box of length n.
-- Will most likely fit, but will quite likely truncate more than necessary.
fitLabelWidth :: TimelineParams -> Double -> Text -> Text
fitLabelWidth _params n s =
    if T.length s <= toTake then s else T.take toTake s <> ".."
  where
    -- one character takes about 5.5 pixels, at most (taking kerning into account)
    alpha = 5.7
    toTake = floor $ n / alpha

-- rectangle with origin in the top-left corner
topLeftRect :: Double -> Double -> D.Diagram B
topLeftRect w h =
  D.rect w h
  & D.moveOriginTo (D.p2 (-w/2, h/2))


-- timeline for a single day
timelineDay :: TimelineParams -> Day -> [(Org, (DiffTime, DiffTime))] -> D.Diagram B
timelineDay params day clocks =
    mconcat
      [ timeticks
      , dateLabel
      , mconcat (map showClock clocks)
      , clocksBackground
      ]
  where
    colWidth = 140 * (params ^. tpColumnWidth)
    colHeight = 700 * (params ^. tpColumnHeight)
    labelWidth = colWidth - 5 -- to account for positioning

    isWeekStartDay =
        dayOfWeek day == (bool Sunday Monday (params ^. tpWeekStartsMonday))
    vSepWidth =
        (if isWeekStartDay then 40 else 17) * (params ^. tpVSepWidth)

    timeticks :: D.Diagram B
    timeticks =
      mconcat $
      flip map [(0::Int)..23] $ \hour ->
      mconcat
        [ D.alignedText 0.5 1 (show hour)
          & D.font "DejaVu Sans"
          & D.fontSize 8
          & D.moveTo (D.p2 (vSepWidth/2, -5))
          & D.fc (D.sRGB24 150 150 150)
        , D.strokeT (D.p2 (0,0) D.~~ (D.p2 (vSepWidth, 0)))
          & D.translateY (-0.5)
          & D.lwO 1
          & D.lc (D.sRGB24 200 200 200)
        ]
      & D.moveTo (D.p2 (0, negate $ fromInteger . round $ colHeight * (fromIntegral hour / 24)))
      & D.moveOriginTo (D.p2 (vSepWidth, 0))

    dateLabel :: D.Diagram B
    dateLabel =
      mconcat
      [ D.strutY 20
      , D.alignedText 0.5 0 (
          T.unpack $
          fitLabelWidth params colWidth (T.pack $ formatTime defaultTimeLocale "%a, %d.%m.%Y" day))
        & D.font "DejaVu Sans"
        & D.fontSize 12
        & D.moveOriginTo (D.p2 (-colWidth/2, 0))
      ]

    clocksBackground :: D.Diagram B
    clocksBackground =
      topLeftRect colWidth colHeight
      & D.lw D.none
      & D.fc (fromMaybe (error "Can't parse colour") $
              parseColour $ params ^. tpBackground)

    contrastFrom c = if luminance c < 0.14 then D.sRGB24 224 224 224 else D.black

    showClock :: (Org, (DiffTime, DiffTime)) -> D.Diagram B
    showClock (org, (start, end)) =
      let
        w = colWidth
        h = (* colHeight) $ fromInteger (diffTimeMinutes $ end - start) / (24*60)
        y = (* colHeight) $ fromInteger (diffTimeMinutes start) / (24*60)

        bgboxColour = entryColour params org
        bgbox = topLeftRect w h
              & D.lw D.none
              & D.fc bgboxColour
        label = D.alignedText 0 0.5 (T.unpack $ fitLabelWidth
                                                   params
                                                   (labelWidth)
                                                   (getTitle $ _orgTitle org))
               & D.font "DejaVu Sans"
               & D.fontSize 10
               & D.fc (contrastFrom bgboxColour)
               & D.moveTo (D.p2 (5, -h/2))
        box = mconcat $ bool [] [label] (fitLabelHeight params 11 h) ++ [bgbox]
      in box & D.moveTo (D.p2 (0, -y))

-- timelines for several days, with top lists
timelineDays
  :: TimelineParams
  -> [Day]
  -> [[(Org, (DiffTime, DiffTime))]]
  -> [[(Org, DiffTime)]]
  -> D.Diagram B
timelineDays params days clocks topLists =
    (D.strutY 10 D.===) $
    D.hcat $
    flip map (days `zip` (clocks `zip` topLists)) $ \(day, (dayClocks, topList)) ->
      D.vsep 5
      [ timelineDay params day dayClocks
      , taskList params (reverse $ sortOn snd topList) False
      ]

-- task list, with durations and colours
taskList :: TimelineParams -> [(Org, DiffTime)] -> Bool -> D.Diagram B
taskList params labels legend =
    D.vsep 5 $ map oneTask labels
  where

    contrastFrom c = if luminance c < 0.14 then D.sRGB24 224 224 224 else D.black

    timeWidth = 26
    underColumnLabelWidth = 140 * (params ^. tpColumnWidth) - timeWidth
    legendColumnWidth = 180 * (params ^. tpLegendColumnWidth)

    oneTask :: (Org, DiffTime) -> D.Diagram B
    oneTask (org, time) =
        let label = getTitle $ _orgTitle org
        in D.hsep 3
             [ mconcat [
                   D.alignedText 1 0.5 (showTime time)
               & D.font "DejaVu Sans"
               & D.fontSize 10
               & D.fc (contrastFrom $ entryColour params org)
               & D.translateX 20
             , D.rect timeWidth 12
               & D.fc (entryColour params org)
               & D.moveTo (D.p2 (9, 0))
               & D.lw D.none]
             , D.alignedText 0 0.5
                 (T.unpack $ (fitLabelWidth
                                params
                                (bool underColumnLabelWidth legendColumnWidth legend)
                                label))
               & D.font "DejaVu Sans"
               & D.fontSize 10
             , D.rect (bool underColumnLabelWidth legendColumnWidth legend) 12
               & D.fc (fromMaybe (error "Can't parse colour") $
                       parseColour $ params ^. tpBackground)
               & D.lw D.none
             ]

    showTime :: DiffTime -> Prelude.String
    showTime time = printf "%d:%02d" hours minutes
      where
        (hours, minutes) = diffTimeMinutes time `divMod` 60

emptyReport :: D.Diagram B
emptyReport =
      mconcat
      [ D.strutY 20
      , D.strutX 40
      , D.alignedText 0.5 0 "empty"
        & D.font "DejaVu Sans"
        & D.fontSize 8
      ]

timelineReport :: TimelineParams -> Org -> TimelineOutput
timelineReport params org =
    if (null $ concat $ org ^.. traverseTree . orgClocks)
    then
      TimelineOutput $ D.vsep 30 [emptyReport]

    else TimelineOutput pic
  where
    lookupDef :: Eq a => b -> a -> [(a, b)] -> b
    lookupDef d a xs = fromMaybe d $ lookup a xs

    -- These two should be taken from the Org itself (min/max).
    (from,to) =
        let c = concat $ org ^.. traverseTree . orgClocks
        in (L.minimum (map cFrom c), L.maximum (map cTo c))

    -- period to show. Right border is -1min, we assume it's non-inclusive
    daysToShow = [localDay from ..
                  localDay ((negate 120 :: Int) `addLocalTime` to)]

    -- unfiltered leaves
    tasks :: [(Org, [Clock])]
    tasks = orgFlatten org

    -- tasks from the given period, split by days
    byDay :: [[(Org, [(DiffTime, DiffTime)])]]
    byDay = selectDays daysToShow tasks

    -- total durations for each task, split by days
    byDayDurations :: [[(Org, DiffTime)]]
    byDayDurations = map totalTimes byDay

    -- total durations for the whole period
    allDaysDurations :: [(Org, DiffTime)]
    allDaysDurations =
      let allEntrys = nub $ map fst $ concat byDayDurations in
      flip map allEntrys $ \entry ->
        (entry,) $ sum $ flip map byDayDurations $ \durations ->
        lookupDef (fromInteger 0) entry durations

    -- split clocks
    clocks :: [[(Org, (DiffTime, DiffTime))]]
    clocks = map allClocks byDay

    -- top list for each day
    topLists :: [[(Org, DiffTime)]]
    topLists =
        map (take (params ^. tpTopDay) . reverse . sortOn (\(_task, time) -> time))
        byDayDurations

    legendItems = take (floor $ (700::Double) / 15) $ reverse $ sortOn snd allDaysDurations
    optLegend | params ^. tpLegend =
                [D.vsep 0 $ [D.strutY 20, taskList params legendItems True]]
              | otherwise = []

    pic = D.hsep 20 $ [ timelineDays params daysToShow clocks topLists ] ++ optLegend

processTimeline :: TimelineParams -> Org -> TimelineOutput
processTimeline params org = timelineReport params org
