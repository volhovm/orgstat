{-# LANGUAGE ScopedTypeVariables #-}

-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import           Universum

import qualified Data.Text         as T
import           Data.Time         (defaultTimeLocale, formatTime, getZonedTime)
import           System.Directory  (createDirectoryIfMissing)
import           System.FilePath   ((</>))
import           System.Wlog       (logDebug, logInfo)
import           Turtle            (shell)

import           OrgStat.Config    (ConfOutput (..), ConfOutputType (..),
                                    OrgStatConfig (..))
import           OrgStat.Helpers   (resolveReport)
import           OrgStat.Outputs   (genBlockOutput, genSummaryOutput, processTimeline,
                                    tpColorSalt, writeReport)
import           OrgStat.WorkMonad (WorkM, wcConfig, wcXdgOpen)

-- | Main application logic.
runOrgStat :: WorkM ()
runOrgStat = do
    conf@OrgStatConfig{..} <- view wcConfig
    logDebug $ "Config: \n" <> show conf

    curTime <- liftIO getZonedTime
    let reportDir = confOutputDir </> formatTime defaultTimeLocale "%F-%H-%M-%S" curTime
    liftIO $ createDirectoryIfMissing True reportDir
    logInfo $ "This report set will be put into: " <> T.pack reportDir

    forM_ confOutputs $ \ConfOutput{..} -> do
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
    whenM (view wcXdgOpen) $ do
        logInfo "Opening reports using xdg-open..."
        void $ shell ("for i in $(ls "<>T.pack reportDir<>"/*); do xdg-open $i; done") empty
    logInfo "Done"
