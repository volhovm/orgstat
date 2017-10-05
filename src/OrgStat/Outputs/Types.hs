-- | Types common among reports.

module OrgStat.Outputs.Types
       ( SVGImageOutput (..)
       , SummaryOutput (..)
       ) where

import           Universum

import           Diagrams.Backend.SVG (B)
import qualified Diagrams.Prelude     as D

-- | SVG timeline image.
newtype SVGImageOutput = SVGImageOutput (D.Diagram B)

-- | Some text (supposed to be single line or something).
newtype SummaryOutput = SummaryOutput Text
