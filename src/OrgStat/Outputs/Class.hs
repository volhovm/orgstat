-- | Common things among reports

module OrgStat.Outputs.Class
       ( ReportOutput (..)
       ) where

import           Universum

import qualified Diagrams.Backend.SVG  as DB
import qualified Diagrams.Prelude      as D
import           System.FilePath       (replaceExtension, (</>))

import           OrgStat.Outputs.Types (SVGImageOutput (..))

-- | Things that reporters output an what we can do with them.
class ReportOutput a where
    -- | Writes report to the disk, given directory and filename.
    writeReport :: (MonadIO m) => FilePath -> FilePath -> a -> m ()

instance ReportOutput SVGImageOutput where
    writeReport dir filename (SVGImageOutput diagram) =
        liftIO $ DB.renderSVG (replaceExtension (dir </> filename) "svg") size diagram
      where
        size = D.dims2D (D.width diagram) (D.height diagram)
