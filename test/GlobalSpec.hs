{-# LANGUAGE ScopedTypeVariables #-}

module GlobalSpec (spec) where

import           Data.Colour.SRGB      (sRGB24show)
import qualified Data.Text             as T
import           Data.Text.Arbitrary   ()
import           Data.Time             (LocalTime (..), addUTCTime, getZonedTime,
                                        localTimeToUTC, utc, utcToLocalTime,
                                        zonedTimeToLocalTime)
import           Data.Time.Calendar    (addGregorianMonthsRollOver)
import           Test.Hspec            (Spec, describe, hspec, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), NonNegative (..),
                                        Positive (..), Small (..), forAll, ioProperty,
                                        oneof, (.&&.), (===), (==>))
import           Universum

import           OrgStat.Config        (ConfDate (..), ConfRange (..))
import           OrgStat.Logic         (convertRange)
import           OrgStat.Util          (parseColour)


data ColorWrapper = ColorWrapper Text deriving Show

instance Arbitrary ColorWrapper where
    arbitrary = do
        nums <- replicateM 6 $ oneof $ map pure "0123456789abcdef"
        pref <- oneof $ map pure ["#", mempty]
        pure $ ColorWrapper $ pref <> T.pack nums

spec :: Spec
spec = do
    parseColourSpec
    convertRangeSpec

parseColourSpec :: Spec
parseColourSpec = describe "Util#parseColour" $ do
    prop "Doesn't fail on correct inputs" $
        forAll arbitrary $ \(ColorWrapper s) ->
        let s' = T.unpack $ if "#" `T.isPrefixOf` s then s else "#" <> s
        in (sRGB24show <$> (parseColour @Text @Double s)) === Just s'
    prop "Fails on incorrect inputs" $
        forAll arbitrary $ \s ->
        length s > 6 ==> (parseColour @Text @Double s) === Nothing

convertRangeSpec :: Spec
convertRangeSpec =  describe "Logic#convertRange" $ do
    curTime <- runIO $ zonedTimeToLocalTime <$> getZonedTime
    let addLocalTime n a = utcToLocalTime utc $ fromIntegral n `addUTCTime` localTimeToUTC utc a
    let subDays a i = (negate i * 60 * 60 * 24) `addLocalTime` a
    let subWeeks a i = (negate i * 60 * 60 * 24 * 7) `addLocalTime` a
    let subMonths a i = a { localDay = (negate i) `addGregorianMonthsRollOver` (localDay a) }
    let inRange c (a,b) = c >= a && c <= b
    let convert = liftIO . convertRange
    prop "(now-1h, now) is correctly parsed" $
        forAll arbitrary $ \(Positive (Small (i :: Integer))) ->
        let hourAgo = (negate i * 60 * 60) `addLocalTime` curTime
        in ioProperty (inRange curTime <$> convert (ConfFromTo (ConfLocal hourAgo) ConfNow))
    prop "Today -N days is exactly in day-N range" $
        forAll arbitrary $ \(NonNegative (Small i)) ->
            ioProperty (inRange (subDays curTime i) <$> convert (ConfBlockDay i)) .&&.
            ioProperty (not . inRange (subDays curTime $ i+1) <$> convert (ConfBlockDay i)) .&&.
            ioProperty (not . inRange (subDays curTime $ i) <$> convert (ConfBlockDay $ i+1))
    prop "Today -N weeks is exactly in week-N range" $
        forAll arbitrary $ \(NonNegative (Small i)) ->
            ioProperty (inRange (subWeeks curTime i) <$> convert (ConfBlockWeek i)) .&&.
            ioProperty (not . inRange (subWeeks curTime $ i+1) <$> convert (ConfBlockWeek i)) .&&.
            ioProperty (not . inRange (subWeeks curTime $ i) <$> convert (ConfBlockWeek $ i+1))
    prop "Today -N months is excatly in month-N range" $
        forAll arbitrary $ \(NonNegative (Small i)) ->
            ioProperty (inRange (subMonths curTime i) <$> convert (ConfBlockMonth i)) .&&.
            ioProperty (not . inRange (subMonths curTime $ i+1) <$> convert (ConfBlockMonth i)) .&&.
            ioProperty (not . inRange (subMonths curTime i) <$> convert (ConfBlockMonth $ i+1))
