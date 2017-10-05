-- | Types common among reports.

module OrgStat.Outputs.Types
       ( SVGImageOutput (..)
       ) where

import           Diagrams.Backend.SVG (B)
import qualified Diagrams.Prelude     as D

-- Also thing to think about is how we output settings (time ranges
-- etc.) -- on the plot, in the corner, in the file name, as a
-- description file ?
newtype SVGImageOutput = SVGImageOutput (D.Diagram B)
