-- | Different utilities

module OrgStat.Util
       ( dropLowerOptions
       , dropEnd
       , fromJustM
       , hashColour
       ) where

import           Control.Lens     (ix, (%~))
import           Data.Aeson.TH    (defaultOptions)
import           Data.Aeson.Types (Options, fieldLabelModifier)
import           Data.Char        (isLower, toLower)
import           Data.Hashable    (hashWithSalt)
import           Data.List        ((!!))
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

-- | Generates a colour given salt an
hashColour :: (Hashable a) => Int -> a -> (Word8, Word8, Word8)
hashColour salt item = colours !! (hashWithSalt salt item `mod` length colours)
  where
    colours = map toColour popularColours
    broken = panic "Util#hashColour is broken"
    toWord8 a b = fromMaybe broken $ readMaybe $ "0x"++[a,b]
    toColour [r1,r2,g1,g2,b1,b2] = (toWord8 r1 r2, toWord8 g1 g2, toWord8 b1 b2)
    toColour _                   = broken
    popularColours :: [[Char]]
    popularColours = ["000000","00FF00","0000FF","FF0000","01FFFE","FFA6FE","FFDB66","006401","010067","95003A","007DB5","FF00F6","FFEEE8","774D00","90FB92","0076FF","D5FF00","FF937E","6A826C","FF029D","FE8900","7A4782","7E2DD2","85A900","FF0056","A42400","00AE7E","683D3B","BDC6FF","263400","BDD393","00B917","9E008E","001544","C28C9F","FF74A3","01D0FF","004754","E56FFE","788231","0E4CA1","91D0CB","BE9970","968AE8","BB8800","43002C","DEFF74","00FFC6","FFE502","620E00","008F9C","98FF52","7544B1","B500FF","00FF78","FF6E41","005F39","6B6882","5FAD4E","A75740","A5FFD2","FFB167","009BFF","E85EBE"];
