-- | Common things among reports

module OrgStat.Report.Class
       ( Report (..)
       ) where

import qualified Diagrams.Backend.SVG as DB
import qualified Diagrams.Prelude     as D
import           Universum

import           OrgStat.Report.Types (SVGImageReport (..))

-- | Things that reporters output an what we can do with them.
class Report a where
    -- | Writes report to the disk.
    writeReport :: (MonadIO m) => FilePath -> a -> m ()

instance Report SVGImageReport where
    writeReport fp (SVGImage (width, height) diagram) =
        liftIO $ DB.renderSVG fp size diagram
      where
        size = (D.mkSizeSpec (D.V2 (Just width) (Just height)))
