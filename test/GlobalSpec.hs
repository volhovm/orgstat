{-# LANGUAGE ScopedTypeVariables #-}

module GlobalSpec (spec) where

import           Data.Colour.SRGB      (sRGB24show)
import qualified Data.Text             as T
import           Test.Hspec            (Spec, describe, hspec, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), forAll, oneof, (===))
import           Universum

import           OrgStat.Util          (parseColour)


data ColorWrapper = ColorWrapper Text deriving Show

instance Arbitrary ColorWrapper where
    arbitrary = do
        nums <- replicateM 6 $ oneof $ map pure "0123456789abcdef"
        pure $ ColorWrapper $ "#" <> T.pack nums

spec :: Spec
spec = do
    describe "Util#parseColour" $ do
        prop "Doesn't fail on correct inputs" $
            forAll arbitrary $ \(ColorWrapper s) ->
            (sRGB24show <$> (parseColour @Text @Double s)) === Just (T.unpack s)
--     curTime <- runIO getCurrentTime
--     prop "Doesn't fail on arbitrary data" $
--         forAll arbitrary $ \(NonNegative len) ->
--         forAll (vectorOf' len $ tweetGen hashtag curTime 1000) $ \tweets ->
--         forAll arbitrary $ \(NonNegative hours) ->
--         (hours > 1 && hours < 13) ==>
--         ioProperty (runStatistics tweets hours >> pure True)
--     prop "Returns correct number of tweets in first hour" $
--         let oneMin = toSeconds (1 :: Minute)
--             oneMinBefore = (- oneMin) `addUTCTime` curTime
--         in forAll arbitrary $ \(NonNegative len) ->
--            forAll (vectorOf' len $ tweetGen hashtag oneMinBefore 10) $ \tweets ->
--            forAll arbitrary $ \(NonNegative hours) ->
--            (hours > 1 && hours < 13) ==>
--            ioProperty ((== len) . last <$> runStatistics (nub tweets) hours)
--     prop "Returns total amount of tweets correctly" $
--         forAll arbitrary $ \(NonNegative len) ->
--         forAll arbitrary $ \(NonNegative hours) ->
--         forAll (vectorOf' len $ tweetGen hashtag curTime (50 * hours)) $ \tweets ->
--         (hours > 1 && hours < 13) ==>
--         ioProperty (do stats <- runStatistics tweets hours
--                        pure $ sum stats == len)
--
--   where
--     hashtag = "#hashtag"
--     runStatistics state h = evalStateT (fromTwitterMock $ getStatistics False hashtag h) state
--     toSeconds unit = fromInteger $ toInteger $ toMicroseconds unit `div` 1000000
