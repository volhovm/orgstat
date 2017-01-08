{-# LANGUAGE RankNTypes #-}
-- | Different utilities

module OrgStat.Util
       ( dropLowerOptions
       , dropEnd
       , fromJustM
       , parseColour
       , hashColour
       , (??~)
       ) where

import           Control.Lens     (ASetter, ix, (%~), (.~))
import           Data.Aeson.TH    (defaultOptions)
import           Data.Aeson.Types (Options, fieldLabelModifier)
import           Data.Char        (isLower, toLower)
import           Data.Hashable    (hashWithSalt)
import           Data.List        (nub)
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

-- | Parses colour from format '#rrggbb' or just 'rrggbb'
parseColour :: forall s. (ToString s) => s -> Maybe (Word8, Word8, Word8)
parseColour (toString -> s) = toColour $ dropWhile (== '#') s
  where
    toColour [r1,r2,g1,g2,b1,b2] = liftA3 (,,) (toWord8 r1 r2) (toWord8 g1 g2) (toWord8 b1 b2)
    toColour _                   = Nothing
    toWord8 a b = readMaybe $ "0x"++[a,b]

-- | Generates a colour given salt an
hashColour :: (Hashable a) => Int -> a -> (Word8, Word8, Word8)
hashColour salt item = colours !! (hashWithSalt salt item `mod` length colours)
  where
    broken = panic "Util#hashColour is broken"
    range = [-5,0,5]
    ac :: Word8 -> Integer -> Word8
    ac a b = fromIntegral $ (fromIntegral a) + b `mod` 255
    colours = nub $ concatMap
        (\(r,g,b) -> [(r `ac` i,g `ac` j,b `ac` k) | i <- range, j <- range, k <- range])
        coloursBasic
    coloursBasic = map (fromMaybe broken . parseColour) popularColours
    popularColours :: [[Char]]
    popularColours = ["00FF00","01FFFE","FFA6FE","FFDB66","006401","010067","95003A","007DB5","FF00F6","FFEEE8","774D00","90FB92","0076FF","D5FF00","FF937E","6A826C","FF029D","FE8900","7A4782","7E2DD2","85A900","FF0056","A42400","00AE7E","683D3B","BDC6FF","263400","BDD393","00B917","9E008E","001544","C28C9F","FF74A3","01D0FF","004754","E56FFE","788231","0E4CA1","91D0CB","BE9970","968AE8","BB8800","43002C","DEFF74","00FFC6","FFE502","620E00","008F9C","98FF52","7544B1","B500FF","00FF78","FF6E41","005F39","6B6882","5FAD4E","A75740","A5FFD2","FFB167","009BFF","E85EBE"];

-- | Maybe setter that does nothing on Nothing.
(??~) :: ASetter s s a b -> Maybe b -> s -> s
(??~) _ Nothing  = identity
(??~) l (Just k) = l .~ k
