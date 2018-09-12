{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | This module defines how report input is formed.

module OrgStat.Scope
       ( AstPath (..)
       , isSubPath
       , existsPath
       , ScopeModifier (..)
       , applyModifiers
       ) where

import qualified Prelude as Base
import Control.Lens (to)
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Universum

import OrgStat.Ast (Org, atDepth, orgClocks, orgSubtrees, orgTags, orgTitle, traverseTree)

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
atPath (AstPath []) f o = fromMaybe o <$> f (Just o)
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

-- | Checks if something is on that path in given 'Org'.
existsPath :: AstPath -> Org -> Bool
existsPath p o = o ^. atPath p . to isJust

-- | Modificicators of org tree. They remove some subtrees
data ScopeModifier
    = ModPruneSubtree AstPath Int
      -- ^ Turns all subtrees starting with @path@ and then on depth @d@ into leaves.
    | ModFilterTag Text
      -- ^ Given text tag name, it leaves only those subtrees that
      -- have this tag (tags are inherited).
    | ModSquash AstPath
      -- ^ Starting at node on path A and depth n, turn A into set of
      -- nodes A/a1/a2/.../an. Doesn't work/make sense for empty path.
    | ModSelectSubtree AstPath
      -- ^ Leaves only node at @path@, deletes all other subtrees.
    deriving (Show,Eq,Ord)

-- | Errors related to modifiers application
data ModifierError
    = MEConflicting ScopeModifier ScopeModifier Text
    -- ^ Modifiers can't be applied together (del/sel)
    | MEWrongParam ScopeModifier Text
    -- ^ Modifier doesn't support this parameter
    deriving (Show,Typeable)

instance Exception ModifierError

-- | Applies modifier to org tree
applyModifier :: ScopeModifier -> Org -> Either ModifierError Org
applyModifier m@(ModPruneSubtree path depth) org = do
    unless (depth >= 0) $ throwError $ MEWrongParam m "Depth should be >= 0"
    unless (existsPath path org) $
        throwError $ MEWrongParam m $ "Path " <> show path <> " doesn't exist"
    let subclocks o' = o' & orgClocks .~ (concatMap (view orgClocks) $ o' ^.. traverseTree)
                          & orgSubtrees .~ []
    let pruneChildren o = o & atDepth depth %~ subclocks
    pure $ org & atPath path %~ (\x -> maybe x (Just . pruneChildren) x)
applyModifier m@(ModSelectSubtree path) org = do
    unless (existsPath path org) $
        throwError $ MEWrongParam m $ "Path " <> show path <> " doesn't exist"
    pure $
        fromMaybe (error "applyModifier@ModSelectSubtree is broken") $
        org ^. atPath path
applyModifier (ModFilterTag tag) o0 = do
    let matchesTag o = any (== tag) (o ^. orgTags)
    let dfs :: Org -> Maybe Org
        dfs o | matchesTag o = Just o
              | otherwise = case mapMaybe dfs (o ^. orgSubtrees) of
                                [] -> Nothing
                                xs -> Just $ o & orgSubtrees .~ xs
    Right $ o0 & orgSubtrees %~ mapMaybe dfs
applyModifier _ org = pure org -- TODO

-- | Generates an org to be processed by report generators from 'Scope'.
applyModifiers :: Org -> [ScopeModifier] -> Either ModifierError Org
applyModifiers org s = do
--    whenList addDelConflicts $ \(m1,m2) ->
--        throwError $ MEConflicting m1 m2 "Path of first modifier is subpath of second one"
    foldrM applyModifier org mods
  where
--    whenList ls foo = case ls of
--        []    -> pass
--        (h:_) -> foo h
--    addDelConflicts =
--        let addDelConflict (ModPruneSubtree a _) (ModSelectSubtree b) = a `isSubPath` b
--            addDelConflict _ _                                        = False
--        in filter (uncurry addDelConflict) modsPairs
--    modsPairs = [(a,b) | a <- mods, b <- mods, a < b]
    mods = sort s
