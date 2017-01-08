-- | Common things among reports

module OrgStat.Report.Class
       ( Report (..)
       ) where

import qualified Diagrams.Backend.SVG as DB
import qualified Diagrams.Prelude     as D
import           System.FilePath      (replaceExtension, (</>))
import           Universum

import           OrgStat.Report.Types (SVGImageReport (..))

-- | Things that reporters output an what we can do with them.
class Report a where
    -- | Writes report to the disk, given directory and filename.
    writeReport :: (MonadIO m) => FilePath -> FilePath -> a -> m ()

instance Report SVGImageReport where
    writeReport dir filename (SVGImage diagram) =
        liftIO $ DB.renderSVG (replaceExtension (dir </> filename) "svg") size diagram
      where
        size = D.dims2D (D.width diagram) (D.height diagram)
