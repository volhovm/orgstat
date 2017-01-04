{-# LANGUAGE TemplateHaskell #-}

-- | This module defines how report input is formed.

module OrgStat.Scope
       ( AstPath (..)
       , isSubPath
       , ScopeModifier (..)
       , Scope (..)
       , sFiles
       , sModifiers
       ) where

import qualified Base         as Base
import           Control.Lens (makeLenses)
import qualified Data.Text    as T
import           Data.Word
import           Universum

import           OrgStat.Ast  (Org)

-- | Path in org AST is just a list of paths, head ~ closer to tree
-- root.
newtype AstPath = AstPath { getAstPath :: [Text] }

instance Base.Show AstPath where
    show (AstPath path)
        | null path = "<null_ast_path>"
        | otherwise = intercalate "/" (map T.unpack path)

isSubPath :: AstPath -> AstPath -> Bool
isSubPath (AstPath l1) (AstPath l2) = l1 `isPrefixOf` l2

-- | Modificicators of org tree. They remove some subtrees
data ScopeModifier
    = ModPruneSubtree AstPath Word
      -- ^ Turns all subtrees starting with @path@ and then on depth @d@ into leaves.
    | ModSelectSubtree AstPath Word
      -- ^ Starting with @path@, it thinks of all children of depth
      -- @n@ as of only leaves to be left in this scope.
    | ModFilterTag Text
      -- ^ Given text tag name, it leaves only those subtrees that
      -- have this tag (tags inherit).
    deriving (Show)

-- | Applies modifier to org tree
applyModifier :: ScopeModifier -> Org -> Org
applyModifier = notImplemented

-- | 'Scope' is just a list of trees, where functional units are
-- leaves. Currently "list item is org-file" isomorphism is dominating
-- in my head, but it can be interpreted in other ways. 'Scope' is
-- thought to be a single unit for plotting. Plotting timeline
-- requires single scope. Plotting activity requires several scope per
-- line. Several activity functions may be plotted on the single
-- sheet, several sheets can be produced.
data Scope = Scope
  { _sFiles     :: HashMap Text Org -- ^ Map from files to org ASTs
  , _sModifiers :: [ScopeModifier] -- ^ Modifiers to apply
  } deriving (Show)

makeLenses ''Scope
