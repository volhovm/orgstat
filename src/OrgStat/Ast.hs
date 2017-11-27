{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Abstract syntax tree for org.

module OrgStat.Ast
       ( Clock (..)
       , Org (..)
       , orgTitle
       , orgTags
       , orgClocks
       , orgSubtrees

       , clockDuration
       , orgTotalDuration
       , filterHasClock
       , cutFromTo
       , fmapOrgLens
       , traverseTree
       , atDepth
       , mergeClocks
       ) where

import Control.Lens (ASetter', makeLenses)
import Data.Time (LocalTime, NominalDiffTime, diffUTCTime, localTimeToUTC, localTimeToUTC, utc)

import Universum

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Org clock representation -- a pair of time in UTC. Should be
-- local time in fact, but we'll assume that UTC timestamps support in
-- org will be added at some point. For now all tags are to be read in
-- local time.
data Clock = Clock
    { cFrom :: LocalTime
    , cTo   :: LocalTime
    } deriving (Show,Eq,Ord)

-- | Main datatype of org AST. It may contain some metadata if needed
-- (e.g. current node depth, children number etc). Content of headers
-- is ignored.
data Org = Org
    { _orgTitle    :: Text
    , _orgTags     :: [Text]
    , _orgClocks   :: [Clock]
    , _orgSubtrees :: [Org]
    } deriving (Show,Eq)

makeLenses ''Org

----------------------------------------------------------------------------
-- Helpers and lenses
----------------------------------------------------------------------------

clockDuration :: Clock -> NominalDiffTime
clockDuration (Clock (localTimeToUTC utc -> from) (localTimeToUTC utc -> to)) =
    diffUTCTime to from

-- | Calculate total clocks duration in org tree.
orgTotalDuration :: Org -> NominalDiffTime
orgTotalDuration org =
    sum $ map clockDuration $ concat $ org ^.. traverseTree . orgClocks

-- | Remove subtrees that have zero total duration.
filterHasClock :: Org -> Org
filterHasClock = orgSubtrees %~ mapMaybe dfs
  where
    dfs :: Org -> Maybe Org
    dfs o = do
        let children = mapMaybe dfs (o ^. orgSubtrees)
        let ownDur = sum $ map clockDuration (o ^. orgClocks)
        if ownDur == 0 && null children
            then Nothing
            else Just $ o & orgSubtrees .~ children

cutFromTo :: (LocalTime, LocalTime) -> Org -> Org
cutFromTo (from, to) o
    | from >= to = error "cutFromTo: from >= to"
    | otherwise = o & traverseTree %~ filterClocks
  where
    -- Cuts clock relatively to from/to or discards if it's not in the
    -- interval.
    fitClock :: Clock -> Maybe Clock
    fitClock Clock{..} = do
        guard $ cFrom <= to
        guard $ cTo >= from
        pure $ Clock (max cFrom from) (min cTo to)

    filterClocks :: Org -> Org
    filterClocks = orgClocks %~ mapMaybe fitClock

-- | Functor-like 'fmap' on field chosen by lens.
fmapOrgLens :: ASetter' Org a -> (a -> a) -> Org -> Org
fmapOrgLens l f o = o & l %~ f & orgSubtrees %~ map (fmapOrgLens l f)

-- | Traverses node and subnodes, all recursively. Bottom-top.
traverseTree :: Traversal' Org Org
traverseTree f o = o'
  where
    o' = liftA2 (\org x -> org & orgSubtrees .~ x) (f o) traversedChildren
    traversedChildren = traverse (traverseTree f) $ o ^. orgSubtrees

atDepth :: Int -> Traversal' Org Org
atDepth i _ o | i < 0 = pure o
atDepth 0 f o = f o
atDepth n f o =
    (\x -> o & orgSubtrees .~ x) <$> traverse (atDepth (n-1) f) (o ^. orgSubtrees)

-- | Merges task clocks that have less then 2m delta between them into
-- one.
mergeClocks :: Org -> Org
mergeClocks = fmapOrgLens orgClocks (mergeClocksDo . sort)
  where
    toUTC = localTimeToUTC utc
    mergeClocksDo [] = []
    mergeClocksDo [x] = [x]
    mergeClocksDo (a:b:xs)
        | toUTC (cFrom b) `diffUTCTime ` toUTC (cTo a) < 2*60 =
          mergeClocksDo $ (Clock (cFrom a) (cTo b)):xs
        | otherwise = a : mergeClocksDo (b:xs)
