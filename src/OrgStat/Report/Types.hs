-- | Types common among reports.

module OrgStat.Report.Types
       ( SVGImageReport (..)
       ) where

import           Universum

-- Also thing to think about is how we output settings (time ranges
-- etc.) -- on the plot, in the corner, in the file name, as a
-- description file ?
data SVGImageReport = SVGImage Void -- wrap real SVG image here.
