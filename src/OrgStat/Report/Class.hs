-- | Common things among reports

module OrgStat.Report.Class
       ( Report (..)
       ) where

import           Universum

import           OrgStat.Report.Types (SVGImageReport (..))

-- | Things that reporters output an what we can do with them.
class Report a where
    -- | Writes report to the disk.
    writeReport :: (MonadIO m) => FilePath -> a -> m ()

instance Report SVGImageReport where
    writeReport _fp (SVGImage _) = notImplemented
