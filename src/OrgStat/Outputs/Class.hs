-- | Common things among reports

module OrgStat.Outputs.Class
       ( ReportOutput (..)
       ) where

import           Universum

import qualified Data.Text.IO          as T
import qualified Diagrams.Backend.SVG  as DB
import qualified Diagrams.Prelude      as D
import           System.FilePath       (replaceExtension)

import           OrgStat.Outputs.Types (BlockOutput (..), SummaryOutput (..),
                                        TimelineOutput (..))

-- | Things that reporters output an what we can do with them.
class ReportOutput a where
    -- | Writes report to the disk, given path to file.
    writeReport :: (MonadIO m) => FilePath -> a -> m ()

instance ReportOutput TimelineOutput where
    writeReport path (TimelineOutput diagram) =
        liftIO $ DB.renderSVG (replaceExtension path "svg") size diagram
      where
        size = D.dims2D (D.width diagram) (D.height diagram)

instance ReportOutput SummaryOutput where
    writeReport path (SummaryOutput text) =
        liftIO $ T.writeFile (replaceExtension path "txt") text

instance ReportOutput BlockOutput where
    writeReport path (BlockOutput text) =
        liftIO $ T.writeFile (replaceExtension path "txt") text
