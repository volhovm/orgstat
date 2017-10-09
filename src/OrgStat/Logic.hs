{-# LANGUAGE ScopedTypeVariables #-}

-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import           Universum

import           Control.Lens      (views)
import qualified Data.Text         as T
import           Data.Time         (defaultTimeLocale, formatTime, getZonedTime)
import           System.Directory  (createDirectoryIfMissing)
import           System.FilePath   ((</>))
import           System.Wlog       (logDebug, logInfo)
import           Turtle            (shell)

import           OrgStat.CLI       (CommonArgs (..))
import           OrgStat.Config    (ConfOutput (..), ConfOutputType (..),
                                    OrgStatConfig (..))
import           OrgStat.Helpers   (resolveOutput, resolveReport)
import           OrgStat.Outputs   (genBlockOutput, genSummaryOutput, processTimeline,
                                    tpColorSalt, writeReport)
import           OrgStat.WorkMonad (WorkM, wcCommonArgs, wcConfig)

-- | Main application logic.
runOrgStat :: WorkM ()
runOrgStat = do
    conf@OrgStatConfig{..} <- view wcConfig
    logDebug $ "Config: \n" <> show conf

    curTime <- liftIO getZonedTime

    let createDir = do
            let reportDir = confOutputDir </>
                            formatTime defaultTimeLocale "%F-%H-%M-%S" curTime
            liftIO $ createDirectoryIfMissing True reportDir
            pure reportDir
    reportDir <- maybe createDir pure =<< views wcCommonArgs outputDir
    logInfo $ "This report set will be put into: " <> fromString reportDir

    outputsToProcess <-
        maybe (pure confOutputs) (fmap one . resolveOutput) =<<
        views wcCommonArgs selectOutput
    forM_ outputsToProcess $ \ConfOutput{..} -> do
        logInfo $ "Processing output " <> coName
        let prePath = reportDir </> T.unpack coName
        case coType of
            TimelineOutput {..} -> do
                resolved <- resolveReport toReport
                let timelineParamsFinal =
                        (confBaseTimelineParams <> toParams) & tpColorSalt .~ confColorSalt
                let res = processTimeline timelineParamsFinal resolved
                logInfo $ "Generating timeline report " <> coName <> "..."
                writeReport prePath res
            SummaryOutput params -> do
                summary <- genSummaryOutput params
                writeReport prePath summary
            BlockOutput {..} -> do
                resolved <- resolveReport boReport
                let res = genBlockOutput boParams resolved
                writeReport prePath res
    whenM (views wcCommonArgs xdgOpen) $ do
        logInfo "Opening reports using xdg-open..."
        void $ shell ("for i in $(ls "<>T.pack reportDir<>"/*); do xdg-open $i; done") empty
    logInfo "Done"
