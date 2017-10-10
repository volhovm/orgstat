-- | Block output similar to default org reporting. This is stub
-- version which is to be improved later.

module OrgStat.Outputs.Block
       ( genBlockOutput
       ) where

import           Universum

import qualified Data.Text             as T

import           OrgStat.Ast           (Org, filterHasClock, orgTitle, orgTotalDuration,
                                        traverseTree)
import           OrgStat.Outputs.Types (BlockOutput (..), BlockParams)
import           OrgStat.Util          (timeF)

-- Stub. Used for debug mostly.
genBlockOutput :: BlockParams -> Org -> BlockOutput
genBlockOutput _ (filterHasClock -> o0) = do
    BlockOutput $ T.unlines $ map formatter (o0 ^.. traverseTree)
  where
    -- todo implement it with boxes package instead, this is just as stub
    cutLen = 50
    margin = 2
    genPad n = fromString (replicate n ' ')
    formatter o =
        let dur = orgTotalDuration o
            titleRaw = T.take cutLen $ o ^. orgTitle
            padding = cutLen - length titleRaw
            titlePadded = titleRaw <> genPad padding
        in mconcat [ titlePadded
                   , genPad margin <> " | " <> genPad margin
                   , timeF dur
                   ]
