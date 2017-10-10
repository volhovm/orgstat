{-# LANGUAGE RankNTypes #-}
-- | Different utilities

module OrgStat.Util
       ( dropLowerOptions
       , dropEnd
       , addLocalTime
       , fromJustM
       , parseColour
       , hashColour
       , (??~)
       , timeF
       ) where

import           Control.Lens     (ASetter, ix)
import           Data.Aeson.TH    (defaultOptions)
import           Data.Aeson.Types (Options, fieldLabelModifier)
import           Data.Char        (isLower, toLower)
import           Data.Colour      (Colour)
import           Data.Colour.CIE  (luminance)
import           Data.Colour.SRGB (RGB (..), sRGB24, toSRGBBounded)
import           Data.Hashable    (hashWithSalt)
import           Data.List        (nub)
import           Data.List        ((!!))
import           Data.Time        (LocalTime (..), NominalDiffTime, addUTCTime,
                                   localTimeToUTC, utc, utcToLocalTime)
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

-- | Same as 'addUTCTime', but for local
addLocalTime :: (Integral n) => n -> LocalTime -> LocalTime
addLocalTime n a =
    utcToLocalTime utc $ fromIntegral n `addUTCTime` localTimeToUTC utc a

-- | Parses colour from format '#rrggbb' or just 'rrggbb'
parseColour :: forall s a. (ToString s, Floating a, Ord a) => s -> Maybe (Colour a)
parseColour (toString -> s) = toColour $ dropWhile (== '#') s
  where
    toColour [r1,r2,g1,g2,b1,b2] = do
        r <- toWord8 r1 r2
        g <- toWord8 g1 g2
        b <- toWord8 b1 b2
        pure $ sRGB24 r g b
    toColour _                   = Nothing
    toWord8 a b = readMaybe $ "0x"++[a,b]

-- | Generates a colour given salt and anything hashable. Doesn't return
-- too dark or too light colors.
hashColour :: (Hashable a) => Int -> a -> Colour Double
hashColour salt item = colours !! (hashWithSalt salt item `mod` length colours)
  where
    broken = error "Util#hashColour is broken"
    range = [-5,0,5]
    colours = filter (\c -> luminance c < 0.8 && luminance c > 0.06) mutate
    ac :: Word8 -> Integer -> Word8
    ac a b = fromIntegral $ (fromIntegral a) + b `mod` 255
    mutate = nub $ concatMap
        (\(r,g,b) -> [sRGB24 (r `ac` i) (g `ac` j) (b `ac` k)
                     | i <- range, j <- range, k <- range])
        coloursBasic
    coloursBasic =
        map (toTriple . fromMaybe broken . parseColour @[Char] @Double) popularColours
    toTriple c = let RGB{..} = toSRGBBounded c in (channelRed, channelGreen, channelBlue)
    popularColours :: [[Char]]
    popularColours = ["00FF00","01FFFE","FFA6FE","FFDB66","006401","010067","95003A","007DB5","FF00F6","FFEEE8","774D00","90FB92","0076FF","D5FF00","FF937E","6A826C","FF029D","FE8900","7A4782","7E2DD2","85A900","FF0056","A42400","00AE7E","683D3B","BDC6FF","263400","BDD393","00B917","9E008E","001544","C28C9F","FF74A3","01D0FF","004754","E56FFE","788231","0E4CA1","91D0CB","BE9970","968AE8","BB8800","43002C","DEFF74","00FFC6","FFE502","620E00","008F9C","98FF52","7544B1","B500FF","00FF78","FF6E41","005F39","6B6882","5FAD4E","A75740","A5FFD2","FFB167","009BFF","E85EBE"];

-- | Maybe setter that does nothing on Nothing.
(??~) :: ASetter s s a b -> Maybe b -> s -> s
(??~) _ Nothing  = identity
(??~) l (Just k) = l .~ k

-- | Time formatter in form HH:MM
timeF :: NominalDiffTime -> Text
timeF n = do
    let totalTimeMin :: Integer
        totalTimeMin = round $ (/ 60) $ toRational n
    let hours = totalTimeMin `div` 60
    let minutes = totalTimeMin `mod` 60
    let showTwo x = (if x < 10 then "0" else "") <> show x
    fromString $ show hours <> ":" <> showTwo minutes
