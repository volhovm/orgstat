{-# LANGUAGE ApplicativeDo, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Universum

import qualified Data.Attoparsec.Text as A
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
        ".gpg" -> (,dropEnd 4 fp) <$> decryptGpg
        ".org" -> (,fp) <$> liftIO (TIO.readFile fp)
        _ -> throwM $ LogicException $
            "File " <> fpt <> " has unknown extension. Need to be .org or .org.gpg"
    let filename = T.pack $ takeBaseName fname
    logDebug $ "Parsing org file " <> fpt
    parsed <-
        case A.parseOnly (O.parseDocument todoKeywords) content of
            Left err  -> throwM $ LogicException $ T.pack err
            Right res -> pure res

    pure (filename, parsed)
  where
    fpt = T.pack fp
    failExternal = throwM . LogicException
    decryptGpg = do
        logDebug $ "Decrypting gpg file: " <> fpt
        (exCode, output) <-
            (procStrict "gpg" ["--quiet", "--decrypt", fpt] empty)
            `catch`
            (\(e :: LogicException) -> failExternal $ "gpg procStrict failed: " <> show e)
        case exCode of
            ExitSuccess   -> pass
            ExitFailure n -> failExternal $ "Gpg failed with code " <> show n
        pure output

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
                (if T.null title then "" else " " <> title)
        in
        let tagList = if null tags then "" else ":" <> T.intercalate ":" tags <> ":" in
        let spaceLen = 77 - T.length prefix - T.length tagList in
        let header = prefix <> fromString (take spaceLen (Universum.repeat ' ')) <> tagList in

        let sec = printSection d section in

        header <> (if T.null sec then "" else "\n" <> sec)

    printSection :: Int -> Section -> Text
    printSection depth Section{..} =
        let indent = fromString (take depth (Universum.repeat ' ')) in
        let Plns plns = sectionPlannings in
        let planningsStr =
                T.intercalate " " $ map (\(k,v) -> show k <> " " <> printTs v) $
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
        let drawersStr = concatMap (\(Drawer name contents) -> withDrawer name [contents]) sectionDrawers in
        T.intercalate ("\n" <> indent) $
          concat [[planningsStr], clocksStr, propsStr, logbookStr, drawersStr, [sectionParagraph]]

    printClock :: Clock -> Text
    printClock (Clock (Just ts, Just (h,m))) =
        let withSpace s = if T.length s == 1 then " " <> s else s in
        let withZero s = if T.length s == 1 then "0" <> s else s in
        "CLOCK: " <> printTs ts <> " => " <> withSpace (show h) <> ":" <> withZero (show m)
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


filterOrg :: [Text] -> LocalTime -> O.Document -> (O.Document, O.Document)
filterOrg doneKeywords archDate Document{..} =
    let hlUpd = map go documentHeadlines in
    (Document { documentHeadlines = map (view _1) hlUpd, ..},
     Document { documentHeadlines = map (view _2) hlUpd, ..})
  where
    tsOlder :: Timestamp -> Bool
    tsOlder ts = case convertDateTime (tsTime ts) of
        Nothing -> False
        Just locTime -> locTime <= archDate

    go :: Headline -> (Headline,Headline,Bool)
    go hl@Headline{..} =
        let subHls = map go subHeadlines in
        -- Whether this headline was CLOSED before archive date
        let toArchive =
                (let isDone = stateKeyword `elem` (map (Just . StateKeyword) doneKeywords) in
                 let Plns plannings = sectionPlannings section in
                 let closedOld = case HM.lookup CLOSED plannings of
                                   Nothing -> True
                                   Just ts -> tsOlder ts in
                 isDone && closedOld) in

         if toArchive && and (map (view _3) subHls)
         then (hl, hl, True)
         else
           let clocks =
                   concatMap (\case Clock (Just x, Just y) -> [(x,y)]
                                    _ -> []) $
                   L.nub $
                   concat [ sectionClocks section
                          , unLogbook (sectionLogbook section)
                          ] in
           let (splitArch, splitRemain) = L.partition (tsOlder . fst) clocks in

           let toClock (a, b) = Clock (Just a, Just b) in

           let removeItem = and (map (view _3) subHls) && splitRemain == [] in
           let hlLeft =
                   hl { section = section { sectionClocks = []
                                          , sectionLogbook = Logbook (map toClock splitRemain)}
                      , subHeadlines = concatMap (\(l,_r,b) -> if b then [] else [l]) subHls } in

           let hlRight =
                   hl { section = section { sectionClocks = []
                                          , sectionLogbook = Logbook (map toClock splitArch)}
                      , subHeadlines = map (view _2) subHls } in

           (hlLeft,hlRight,removeItem)


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
        TIO.writeFile (argsOutDir </> ("remain_" <> toString fn)) $ printDocument remain
        TIO.writeFile (argsOutDir </> ("archive_" <> toString fn)) $ printDocument arch

    logInfo "mda"
