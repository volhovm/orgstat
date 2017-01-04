-- | Abstract syntax tree for org.

module OrgStat.Ast
       ( Org
       ) where

import           Universum

-- | Main datatype of org AST. It may contain some metadata if needed
-- (e.g. current node depth, children number etc).
data Org = TODO
         deriving (Show)
