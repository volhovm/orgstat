-- | Summary output type.

module OrgStat.Outputs.Summary
       ( genSummaryOutput
       ) where

import           Universum

import           OrgStat.Outputs.Types (SummaryOutput (..), SummaryParams (..))
import           OrgStat.WorkMonad     (WorkM)

-- | Generates summary using provided params.
genSummaryOutput :: SummaryParams -> WorkM SummaryOutput
genSummaryOutput SummaryParams{..} = do
    pure $ SummaryOutput "you're using org-mode probably"
