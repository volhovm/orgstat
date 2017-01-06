-- | Different utilities

module OrgStat.Util
       ( dropLowerOptions
       , dropEnd
       , fromJustM
       ) where

import           Control.Lens     (ix, (%~))
import           Data.Aeson.TH    (defaultOptions)
import           Data.Aeson.Types (Options, fieldLabelModifier)
import           Data.Char        (isLower, toLower)
import           Universum

-- | JSON/Yaml TH modifier. Each field of type "aoeuKek" turns into
-- "kek". Placed here because it can't be defined near json TH
-- deriving (ghc restriction).
dropLowerOptions :: Options
dropLowerOptions =
    defaultOptions
    { fieldLabelModifier = \x -> (dropWhile isLower x) & ix 0 %~ toLower
    }

fromJustM :: Monad m => m b -> m (Maybe b) -> m b
fromJustM e m = maybe e pure =<< m

-- | Drops n items from the end.
dropEnd :: Int -> [x] -> [x]
dropEnd n xs = take (length xs - n) xs
