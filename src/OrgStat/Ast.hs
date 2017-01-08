{-# LANGUAGE TemplateHaskell #-}

-- | Abstract syntax tree for org.

module OrgStat.Ast
       ( Clock (..)
       , Org (..)
       , orgTitle
       , orgTags
       , orgClocks
       , orgSubtrees
       , fmapOrgLens
       , mergeClocks
       ) where

import           Control.Lens    (ASetter', makeLenses, (%~))
import           Data.Time.Clock (UTCTime, diffUTCTime)

import           Universum

-- | Org clock representation -- a pair of time in UTC. Should be
-- local time in fact, but we'll assume that UTC timestamps support in
-- org will be added at some point. For now all tags are to be read in
-- local time.
data Clock = Clock
    { cFrom :: UTCTime
    , cTo   :: UTCTime
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

fmapOrgLens :: ASetter' Org a -> (a -> a) -> Org -> Org
fmapOrgLens l f o = o & l %~ f & orgSubtrees %~ map (fmapOrgLens l f)

-- | Merges task clocks that have less then 1m delta between them into
-- one.
mergeClocks :: Org -> Org
mergeClocks = fmapOrgLens orgClocks (mergeClocksDo . sort)
  where
    mergeClocksDo [] = []
    mergeClocksDo [x] = [x]
    mergeClocksDo (a:b:xs)
        | diffUTCTime (cFrom b) (cTo a) <= 1 =
          Clock (cFrom a) (cTo b) : mergeClocksDo xs
        | otherwise = a : mergeClocksDo (b:xs)
