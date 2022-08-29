-- | Block output similar to default org reporting. This is stub
-- version which is to be improved later.

module OrgStat.Outputs.Block
       ( genBlockOutput
       ) where

import Universum

import qualified Data.List as L
import qualified Data.Text as T
import Text.PrettyPrint.Boxes (center1, hsep, left, render, right, text, vcat)

import OrgStat.Ast (Org, filterHasClock, orgSubtrees, orgTitle, orgTotalDuration, Title(..))
import OrgStat.Outputs.Types (BlockOutput(..), BlockParams(..))
import OrgStat.Util (dropEnd, timeF)

data BlockFrames = BlockFrames
    { bfAngle1     :: Text
    , bfAngle2     :: Text
    , bfHorizontal :: Text
    , bfVertical   :: Text
    } deriving Show

unicodeBlockFrames,asciiBlockFrames :: BlockFrames
unicodeBlockFrames = BlockFrames "├" "└" "─" "│"
asciiBlockFrames = BlockFrames "|" "\\" "-" "|"

-- | Generate block output (emacs table-like).
genBlockOutput :: BlockParams -> Org -> BlockOutput
genBlockOutput BlockParams{..} (filterHasClock -> o0) = do
    BlockOutput $ fromString $ render $
        hsep 2 center1 [vsep,col1,vsep,col2,vsep]
  where
    BlockFrames{..} = if _bpUnicode then unicodeBlockFrames else asciiBlockFrames
    text' = text . toString
    elems' = withDepth (0::Int) o0
    col1 = vcat left $ map (text' . trimTitle . fst) elems'
    col2 = vcat right $ map (text' . snd) elems'
    vsep = vcat center1 $ replicate (length elems') (text $ toString bfVertical)

    trimTitle t | T.length t > _bpMaxLength = T.take (_bpMaxLength - 3) t <> "..."
                | otherwise = t
    formatter o =
        let dur = orgTotalDuration o
            titleRaw = T.take _bpMaxLength $ getTitle $ o ^. orgTitle
        in (titleRaw, timeF dur)

    withDepth :: Int -> Org -> [(Text,Text)]
    withDepth i o = do
        let (name,dur) = formatter o
        let children = map (withDepth (i+1)) (o ^. orgSubtrees)
        let processChild,processLastChild :: [(Text,Text)] -> [(Text,Text)]
            processChild [] = []
            processChild (pair0:pairs) =
                first ((bfAngle1 <> bfHorizontal <> " ") <>) pair0 :
                map (first ((bfVertical <> "  ") <>)) pairs
            processLastChild [] = []
            processLastChild (pair0:pairs) =
                first ((bfAngle2 <> bfHorizontal <> " ") <>) pair0 :
                map (first ("   " <>)) pairs
        let childrenProcessed
                | null children = []
                | otherwise =
                      concat $
                      map processChild (dropEnd 1 children) ++
                      [processLastChild (L.last children)]
        (name,dur) : childrenProcessed
