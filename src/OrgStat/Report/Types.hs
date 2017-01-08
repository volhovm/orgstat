-- | Types common among reports.

module OrgStat.Report.Types
       ( SVGImageReport (..)
       ) where

import           Diagrams.Backend.SVG (B)
import qualified Diagrams.Prelude     as D

-- Also thing to think about is how we output settings (time ranges
-- etc.) -- on the plot, in the corner, in the file name, as a
-- description file ?
data SVGImageReport = SVGImage (D.Diagram B)
