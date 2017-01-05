{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines how report input is formed.

module OrgStat.Scope
       ( AstPath (..)
       , isSubPath
       , existsPath
       , ScopeModifier (..)
       , Scope (..)
       , sRoot
       , sModifiers
       , fromScope
       ) where

import qualified Base                 as Base
import           Control.Lens         (Lens', makeLenses, to, view, (%~), (.~), (^.))
import           Control.Monad.Except (throwError)
import qualified Data.Text            as T
import           Universum

import           OrgStat.Ast          (Org, orgSubtrees, orgTitle)

-- | Path in org AST is just a list of paths, head ~ closer to tree
-- root.
newtype AstPath = AstPath
    { getAstPath :: [Text]
    } deriving (Eq, Ord)

instance Base.Show AstPath where
    show (AstPath path)
        | null path = "<null_ast_path>"
        | otherwise = intercalate "/" (map T.unpack path)

isSubPath :: AstPath -> AstPath -> Bool
isSubPath (AstPath l1) (AstPath l2) = l1 `isPrefixOf` l2

-- | Lens to a org node at path.
atPath :: AstPath -> Lens' Org (Maybe Org)
atPath (AstPath p) f o = atPathDo p o
  where
    atPathDo [] org = f Nothing $> org
    atPathDo (x:xs) org =
        let match = find ((== x) . view orgTitle) $ org ^. orgSubtrees
            modified foo = org & orgSubtrees %~ foo . filter ((/= match) . Just)
            fmapFoo Nothing   = modified identity
            fmapFoo (Just o') = modified (o' :)
        in case (xs,match) of
            (_,Nothing)   -> f Nothing $> org
            ([],_)        -> fmap fmapFoo $ f match
            (cont,Just m) -> fmap (\new -> modified (new:)) $ atPathDo cont m

existsPath :: AstPath -> Org -> Bool
existsPath p o = o ^. atPath p . to isJust

-- | Modificicators of org tree. They remove some subtrees
data ScopeModifier
    = ModPruneSubtree AstPath Int
      -- ^ Turns all subtrees starting with @path@ and then on depth @d@ into leaves.
    | ModFilterTag Text
      -- ^ Given text tag name, it leaves only those subtrees that
      -- have this tag (tags inherit).
    | ModSquash AstPath
      -- ^ Starting at node on path A and depth n, turn A into set of
      -- nodes A/a1/a2/.../an. Doesn't work/make sense for empty path.
    | ModSelectSubtree AstPath
      -- ^ Leaves only node at @path@, deletes all other subtrees.
    deriving (Show,Eq,Ord)

-- | Errors related to modifiers application
data ModifierError
    = MEConflicting ScopeModifier ScopeModifier
    -- ^ Modifiers can't be applied together (del/sel)
    | MEWrongParam ScopeModifier Text
    -- ^ Modifier doesn't support this parameter
    deriving (Show,Typeable)

-- | Applies modifier to org tree
applyModifier :: ScopeModifier -> Org -> Either ModifierError Org
applyModifier m@(ModPruneSubtree path depth) org = do
    unless (depth >= 0) $ throwError $ MEWrongParam m "Depth should be >= 0"
    unless (existsPath path org) $
        throwError $ MEWrongParam m $ "Path " <> show path <> " doesn't exist"
    pure $ org & atPath path .~ Nothing
applyModifier _ org = pure org -- TODO


-- | 'Scope' is just a list of trees, where functional units are
-- leaves. 'Scope' is thought to be a single unit for
-- plotting. Plotting timeline requires single scope. Plotting
-- activity report requires several scope -- each one represents
-- function. Top-level node represents, well, nothing, and her
-- children are files.
data Scope = Scope
  { _sRoot      :: Org
  , _sModifiers :: [ScopeModifier] -- ^ Modifiers to apply
  } deriving (Show)

makeLenses ''Scope

-- | Generates an org to be processed by report generators from 'Scope'.
fromScope :: Scope -> Either ModifierError Org
fromScope s = foldrM applyModifier (s ^. sRoot) mods
  where
    mods = s ^. sModifiers . to sort
