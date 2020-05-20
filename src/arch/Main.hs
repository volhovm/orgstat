{-# LANGUAGE ApplicativeDo, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Universum

import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.OrgMode.Parse as O
import Data.OrgMode.Types
import qualified Data.OrgMode.Types as O
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
  (LocalTime(..), NominalDiffTime, TimeOfDay(..), diffUTCTime, fromGregorian, localTimeToUTC, utc)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Version (showVersion)
import Options.Applicative.Simple
  (Parser, ReadM, help, long, maybeReader, metavar, option, simpleOptions, strOption, switch,
  value)
import Paths_orgstat (version)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeExtension)
import System.FilePath ((</>))
import Turtle (ExitCode(..), procStrict)

import OrgStat.CLI (CommonArgs, parseCommonArgs)
import OrgStat.Logging (Severity(..), initLogging, logDebug, logError, logInfo)
import OrgStat.Logic (runOrgStat)
import OrgStat.Util (dropEnd)
import OrgStat.WorkMonad (WorkConfig(..), runWorkM)

----------------------------------------------------------------------------
-- Arguments/options
----------------------------------------------------------------------------

data Args = Args
    { argsInputs       :: ![String]
    , argsOutDir       :: !String
    , argsTodoKeywords :: ![Text]
    , argsDoneKeywords :: ![Text]
    , argsDate         :: !LocalTime
    , argsDebug        :: !Bool
    } deriving Show

argsParser :: Parser Args
argsParser = do
    argsInputs <-
        some $
        strOption
            (long "input" <> metavar "FILEPATH" <>
            help "Files to consider for input")
    argsOutDir <- strOption (long "out-dir" <> metavar "FILENAME" <> help "Output directory")
    argsTodoKeywords <- option (maybeReader $ pure . map fromString . L.words)
                               (long "todo-keywords" <> metavar "STRLIST" <> help "TODO keywords, space-separated list")
    argsDoneKeywords <- option (maybeReader $ pure . map fromString . L.words)
                               (long "done-keywords" <> metavar "STRLIST" <> help "DONE keywords, space-separated list")
    argsDate <- option dateReadM (long "archive-from" <> metavar "DATE <YYYY-MM-DD>")
    argsDebug <- switch (long "debug" <> help "Enable debug logging")
    pure Args{..}
  where
    dateReadM :: ReadM LocalTime
    dateReadM = maybeReader $ \s ->
        case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
            Nothing -> fail $
                "Couldn't read date " <> show s <>
                ". Correct format is YYYY-MM-DD"
            Just ut -> pure ut


getOptions :: IO Args
getOptions = do
    (res, ()) <-
        simpleOptions
            ("orgarch-" <> showVersion version)
            "----- OrgArch ------"
            "The tool for creating org-mode archives"
            argsParser
            empty
    pure res


----------------------------------------------------------------------------
-- Parsing and processing
----------------------------------------------------------------------------

data LogicException = LogicException Text
    deriving (Show, Typeable)

instance Exception LogicException


-- | Analogous to readOrgFile from OrgStat.IO, but with original
-- orgmode-parse datatype.
readOrgFile
    :: (MonadIO m, MonadCatch m)
    => [Text] -> FilePath -> m (Text, O.Document)
readOrgFile todoKeywords fp = do
    logDebug $ "Reading org file " <> fpt
    unlessM (liftIO $ doesFileExist fp) $
        throwM $ LogicException $ "Org file " <> fpt <> " doesn't exist"
    (content, fname) <- case takeExtension fp of
        ".org" -> (,fp) <$> liftIO (TIO.readFile fp)
        _ -> throwM $ LogicException $
            "File " <> fpt <> " has unknown extension. Need to be .org"
    let filename = T.pack $ takeBaseName fname
    logDebug $ "Parsing org file " <> fpt
    parsed <-
        case A.parseOnly (O.parseDocument todoKeywords) content of
            Left err  -> throwM $ LogicException $ T.pack err
            Right res -> pure res

    pure (filename, parsed)
  where
    fpt = T.pack fp

-- Nothing for DateTime without time-of-day
convertDateTime :: O.DateTime -> Maybe LocalTime
convertDateTime
    O.DateTime
      { yearMonthDay = O.YearMonthDay year month day
      , hourMinute = Just (hour, minute)
      }
  = Just $ LocalTime
      (fromGregorian (toInteger year) month day)
      (TimeOfDay hour minute 0)
convertDateTime _ = Nothing


printDocument :: Document -> Text
printDocument Document{..} =
    documentText <> "\n" <> T.intercalate "\n" (map printHeadline documentHeadlines)
  where
    prependSpace prefix s = if T.null s then "" else prefix <> s

    printStats (StatsPct i) = " [" <> show i <> "%]"
    printStats (StatsOf i j) = " [" <> show i <> "/" <> show j <> "]"

    printHeadline :: Headline -> Text
    printHeadline Headline{..} =
        let Depth d = depth in
        let prefix =
                fromString (take d (Universum.repeat '*')) <>
                maybe "" (\s -> " " <> unStateKeyword s) stateKeyword <>
                maybe "" (\p -> " [#" <> show p <> "]") priority <>
                maybe "" printStats  stats <>
                (prependSpace " " title)
        in
        let tagList = if null tags then "" else ":" <> T.intercalate ":" tags <> ":" in
        let spaceLen = 77 - T.length prefix - T.length tagList in
        let header = prefix <>
                prependSpace (fromString (take spaceLen (Universum.repeat ' '))) tagList in

        let sec = printSection d section in

        let subHeadlinesStr = map printHeadline subHeadlines in

        header <> (prependSpace "\n" sec) <>
                  (if all T.null subHeadlinesStr
                   then "" else "\n" <> T.intercalate "\n" subHeadlinesStr)

    printSection :: Int -> Section -> Text
    printSection depth Section{..} =
        let indent = fromString (take (depth+1) (Universum.repeat ' ')) in
        let Plns plns = sectionPlannings in
        let planningsStr =
                T.intercalate " " $ map (\(k,v) -> show k <> ": " <> printTs v) $
                HM.toList plns in
        let clocksStr = map printClock sectionClocks in
        let withDrawer name xs = [":" <> name <> ":"] <> xs <> [":END:"] in
        let props = unProperties sectionProperties in
        let propsStr = if HM.null props
                       then []
                       else withDrawer "PROPERTIES" $
                            map (\(k,v) -> ":" <> k <> ": " <> v) $ HM.toList props in
        let logbook = unLogbook sectionLogbook in
        let logbookStr = if logbook == [] then [] else
                         withDrawer "LOGBOOK" $ map printClock logbook in
        let drawersStr =
                concatMap (\(Drawer name contents) -> withDrawer name [contents]) sectionDrawers in

        let maybeInclude s = if T.null s then [] else [s] in
        prependSpace indent $
          T.intercalate ("\n" <> indent)
          (concat [ maybeInclude planningsStr
                  , clocksStr, propsStr
                  , logbookStr, drawersStr])
                  <> T.stripEnd sectionParagraph

    printClock :: Clock -> Text
    printClock (Clock (Just ts, Just (h,m))) =
        let withSpace s = if T.length s == 1 then " " <> s else s in
        let withZero s = if T.length s == 1 then "0" <> s else s in
        "CLOCK: " <> printTs ts <> " => " <> withSpace (show h) <> ":" <> withZero (show m)
    printClock (Clock (Just ts, Nothing)) = "CLOCK: " <> printTs ts
    printClock c = error $ "printClock: not yet implemented " <> show c

    printTs :: Timestamp -> Text
    printTs Timestamp{..} =
        bracketOnActive tsActive (dateTime tsTime) <>
        maybe "" (\end -> "--" <> bracketOnActive tsActive (dateTime end)) tsEndTime
      where
        maybePrepend a s = if s == "" then a else a <> " " <> s
        withZero s = if T.length s == 1 then "0" <> s else s
        dateTime DateTime{..} =
          (show (ymdYear yearMonthDay) <> "-" <>
           withZero (show (ymdMonth yearMonthDay)) <> "-" <>
           withZero (show (ymdDay yearMonthDay))) `maybePrepend`
          maybe "" id dayName `maybePrepend`
          maybe "" (\(h,m) -> withZero (show h) <> ":" <> withZero (show m)) hourMinute `maybePrepend`
          maybe "" printRepeater repeater `maybePrepend`
          maybe "" printDelay delay

        bracketOnActive active s = if active == Active then "<" <> s <> ">" else "[" <> s <> "]"

        printTimeUnit = \case
            UnitYear -> "y"
            UnitMonth -> "m"
            UnitWeek -> "w"
            UnitDay -> "d"
            UnitHour -> "h"

        printRepeater Repeater{..} =
            (case repeaterType of
               RepeatCumulate -> "++"
               RepeatCatchUp -> "+"
               RepeatRestart -> ".+") <>
            show repeaterValue <>
            printTimeUnit repeaterUnit

        printDelay Delay{..} =
            (case delayType of
               DelayAll -> "-"
               DelayFirst -> "--") <>
            show delayValue <>
            printTimeUnit delayUnit

-- TODO: repeated common tasks?
filterOrg :: [Text] -> LocalTime -> O.Document -> (O.Document, O.Document)
filterOrg doneKeywords archDate Document{..} =
    let hlUpd = map go documentHeadlines in
    (Document { documentHeadlines = mapMaybe fst hlUpd, ..},
     Document { documentHeadlines = mapMaybe snd hlUpd, ..})
  where
    tsOlder :: Timestamp -> Bool
    tsOlder ts = case convertDateTime (tsTime ts) of
        Nothing -> False
        Just locTime -> locTime <= archDate

    go :: Headline -> (Maybe Headline,Maybe Headline)
    go hl@Headline{..} =
         let subHls = map go subHeadlines in
         -- Whether this headline was CLOSED before archive date
         let isDone = stateKeyword `elem` (map (Just . StateKeyword) doneKeywords) in
         let Plns plannings = sectionPlannings section in
         let isRepeating = case HM.lookup SCHEDULED plannings of
               Nothing -> False
               Just ts -> isJust (repeater (tsTime ts)) in
         let closedOld = case HM.lookup CLOSED plannings of
               Nothing -> True
               Just ts -> tsOlder ts in

         if isRepeating || stateKeyword == Nothing
         then
           let clocks =
                   reverse $
                   L.sortOn (maybe (error "Couldn't convert datetime to sort") id .
                             convertDateTime . tsTime . fst) $
                   map (\case Clock (Just x, y) -> (x,y)
                              c -> error $ "Have encountered a broken clock: " <> show c) $
                   L.nub $
                   concat [ sectionClocks section
                          , unLogbook (sectionLogbook section)
                          ] in
           let (splitArch, splitRemain) = L.partition (tsOlder . fst) clocks in

           let toClock (a, b) = Clock (Just a, b) in

           let remainItem = all (isNothing . snd) subHls && splitArch == [] in
           let hlLeft =
                   hl { section = section { sectionClocks = []
                                          , sectionLogbook = Logbook (map toClock splitRemain)}
                      , subHeadlines = mapMaybe fst subHls } in

           let hlRight =
                   hl { section = section { sectionClocks = []
                                          , sectionLogbook = Logbook (map toClock splitArch)}
                      , subHeadlines = mapMaybe snd subHls } in

           (Just hlLeft,if remainItem then Nothing else Just hlRight)
         else (if isDone && closedOld then (Nothing, Just hl) else (Just hl, Nothing))


main :: IO ()
main = do
    args@Args{..} <- getOptions
    let sev = if argsDebug then Debug else Info
    initLogging sev
    logInfo $ show args

    orgFiles <- forM argsInputs $ readOrgFile $ argsTodoKeywords <> argsDoneKeywords

    logInfo $ show $ map fst orgFiles

    let filteredFiles = map (second $ filterOrg argsDoneKeywords argsDate) orgFiles

    forM_ filteredFiles $ \(fn,(remain,arch)) -> do
        TIO.writeFile (argsOutDir </> (toString fn <> "_remain.org" )) $ printDocument remain
        TIO.writeFile (argsOutDir </> (toString fn <> "_archive.org")) $ printDocument arch

    logInfo "Done"
