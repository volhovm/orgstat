{-# LANGUAGE TemplateHaskell #-}

-- | Abstract syntax tree for org.

module OrgStat.Ast
       ( Clock (..)
       , Org (..)
       ) where

import           Control.Lens    (makeLenses)
import           Data.Time.Clock (UTCTime)

import           Universum

-- | Org clock representation -- a pair of time in UTC. Should be
-- local time in fact, but we'll assume that UTC timestamps support in
-- org will be added at some point. For now all tags are to be read in
-- local time.
data Clock = Clock
    { cFrom :: UTCTime
    , cTo   :: UTCTime
    } deriving (Show)

-- | Main datatype of org AST. It may contain some metadata if needed
-- (e.g. current node depth, children number etc). Content of headers
-- is ignored.
data Org = Org
    { _orgTitle    :: Text
    , _orgTags     :: [Text]
    , _orgClocks   :: [Clock]
    , _orgSubtrees :: [Org]
    } deriving (Show)

makeLenses ''Org
