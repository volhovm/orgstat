{-# LANGUAGE ScopedTypeVariables #-}

-- | Main logic combining all components

module OrgStat.Logic
       ( runOrgStat
       ) where

import Universum

import Control.Lens (views)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Turtle (shell)

import OrgStat.CLI (CommonArgs (..))
import OrgStat.Config (ConfOutput (..), ConfOutputType (..), OrgStatConfig (..))
import OrgStat.Helpers (resolveOutput, resolveReport)
import OrgStat.Logging (logDebug, logInfo)
import OrgStat.Outputs (genBlockOutput, genSummaryOutput, processScriptOutput, processTimeline,
                        writeReport)
import OrgStat.WorkMonad (WorkM, wcCommonArgs, wcConfig)

-- | Main application logic.
runOrgStat :: WorkM ()
runOrgStat = do
    conf@OrgStatConfig{..} <- view wcConfig
    logDebug $ "Config: \n" <> show conf

    curTime <- liftIO getZonedTime
    let getOutputDir = do
            let createDir = do
                    let reportDir =
                            confOutputDir </>
                            formatTime defaultTimeLocale "%F-%H-%M-%S" curTime
                    liftIO $ createDirectoryIfMissing True reportDir
                    pure reportDir
            maybe createDir pure =<< views wcCommonArgs caOutputDir
    let writeOutput coName res = do
            reportDir <- getOutputDir
            let prePath = reportDir </> T.unpack coName
            logInfo $ "This output will be written into: " <> fromString reportDir
            writeReport prePath res

    cliOuts <- views wcCommonArgs caOutputs
    outputsToProcess <-
        bool (mapM resolveOutput cliOuts) (pure confOutputs) (null cliOuts)

    forM_ outputsToProcess $ \ConfOutput{..} -> do
        logInfo $ "Processing output " <> coName
        case coType of
            TimelineOutput {..} -> do
                resolved <- resolveReport toReport
                let timeline = processTimeline confTimelineParams resolved
                logDebug $ "Generating timeline report " <> coName <> "..."
                writeOutput coName timeline
            SummaryOutput params -> do
                summary <- genSummaryOutput params
                writeOutput coName summary
            ScriptOutput params ->
                processScriptOutput params
            BlockOutput {..} -> do
                resolved <- resolveReport boReport
                let res = genBlockOutput boParams resolved
                writeOutput coName res

    whenM (views wcCommonArgs caXdgOpen) $ do
        reportDir <- getOutputDir
        logInfo "Opening reports using xdg-open..."
        void $ shell ("for i in $(ls "<>T.pack reportDir<>"/*); do xdg-open $i; done") empty
    logInfo "Done"
