{-# LANGUAGE ScopedTypeVariables #-}

module GlobalSpec (spec) where

import           Control.Lens          (to, (^.))
import           Data.Colour.SRGB      (sRGB24show)
import qualified Data.Text             as T
import           Data.Text.Arbitrary   ()
import           Data.Time             (LocalTime (..), TimeOfDay (..), getZonedTime,
                                        zonedTimeToLocalTime)
import           Data.Time.Calendar    (addGregorianMonthsRollOver, fromGregorian)
import           Test.Hspec            (Spec, describe, hspec, pending, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), Gen, NonNegative (..),
                                        Positive (..), Small (..), choose, forAll,
                                        ioProperty, oneof, (.&&.), (===), (==>))
import           Universum
import           Unsafe                (unsafeTail)

import           OrgStat.Ast           (Clock (..), Org (..), mergeClocks, orgClocks)
import           OrgStat.Config        (ConfDate (..), ConfRange (..))
import           OrgStat.Logic         (convertRange)
import           OrgStat.Util          (addLocalTime, parseColour)


spec :: Spec
spec = do
    astSpec
    parseColourSpec
    convertRangeSpec

----------------------------------------------------------------------------
-- AST
----------------------------------------------------------------------------

instance Arbitrary LocalTime where
    arbitrary = do
        d <- choose (1, 28)
        m <- choose (1, 12)
        y <- choose (2015, 2016)
        todHour <- choose (0, 23)
        todMin <- choose (0, 59)
        todSec <- fromIntegral <$> (choose (0, 60) :: Gen Int)
        return $ LocalTime (fromGregorian y m d) TimeOfDay{..}

newtype OrgWrapper = OrgWrapper Org deriving Show

-- Generates an org file with almost-fitting intervals that should be
-- merged in one
contOrg :: Int -> Int -> Gen Org
contOrg dfrom dto = do
    (Positive i) <- arbitrary
    (checkpoints :: [LocalTime]) <- sort <$> replicateM (i+1) arbitrary
    pairs <- forM (checkpoints `zip` unsafeTail checkpoints) $ \(c,c') -> do
        delta <- choose (dfrom,dto)
        pure (c, negate delta `addLocalTime` c')
    let clocks = map (uncurry Clock) pairs
    pure $ Org "ShouldHaveOneSubclock" [] clocks []

astSpec :: Spec
astSpec = do
    describe "Ast#mergeClocks" $ do
        prop "merges a set of clocks when needed into one" $
            forAll (contOrg 0 90) $ \o ->
            mergeClocks o ^. orgClocks . to length === 1
        prop "doesn't modify big deltas" $
            forAll (contOrg 120 240) $ \o -> mergeClocks o === o

----------------------------------------------------------------------------
-- Ranges
----------------------------------------------------------------------------

convertRangeSpec :: Spec
convertRangeSpec = describe "Logic#convertRange" $ do
    curTime <- runIO $ zonedTimeToLocalTime <$> getZonedTime
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


----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

newtype ColorWrapper = ColorWrapper Text deriving Show

instance Arbitrary ColorWrapper where
    arbitrary = do
        nums <- replicateM 6 $ oneof $ map pure "0123456789abcdef"
        pref <- oneof $ map pure ["#", mempty]
        pure $ ColorWrapper $ pref <> T.pack nums

parseColourSpec :: Spec
parseColourSpec = describe "Util#parseColour" $ do
    prop "Doesn't fail on correct inputs" $
        forAll arbitrary $ \(ColorWrapper s) ->
        let s' = T.unpack $ if "#" `T.isPrefixOf` s then s else "#" <> s
        in (sRGB24show <$> (parseColour @Text @Double s)) === Just s'
    prop "Fails on incorrect inputs" $
        forAll arbitrary $ \s ->
        length s > 6 ==> (parseColour @Text @Double s) === Nothing
