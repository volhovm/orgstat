-- | Basic logging, copied from log-warper (it has too many
-- dependencies).
module OrgStat.Logging
    (
      Severity (..)
    , setLoggingSeverity

    , logDebug
    , logInfo
    , logNotice
    , logWarning
    , logError

    , logMessage
    ) where

import Universum

import Control.Concurrent.MVar (modifyMVar_, withMVar)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Fmt (fmt, padRightF, (+|), (|+), (|++|))
import Fmt.Time (dateDashF, hmsF, subsecondF, tzNameF)
import System.Console.ANSI
  (Color(Blue, Green, Magenta, Red, Yellow), ColorIntensity(Vivid), ConsoleLayer(Foreground),
  SGR(Reset, SetColor), setSGRCode)
import System.IO.Unsafe (unsafePerformIO)


-- | Severity is level of log message importance. It uniquely
-- determines which messages to print.
data Severity
    = Debug        -- ^ Debug messages
    | Info         -- ^ Information
    | Notice       -- ^ Important (more than average) information
    | Warning      -- ^ General warnings
    | Error        -- ^ General errors/severe errors
    deriving (Eq, Ord, Show)

-- | Internal information about logging.
data LogInternalState = LogInternalState
    { lisMinSeverity :: Severity
    } deriving Show

-- | Internal logging state. Default is Info.
{-# NOINLINE loggingState #-}
loggingState :: MVar LogInternalState
loggingState = unsafePerformIO $ newMVar $ LogInternalState Info

-- | Set logging severity to the given level. Default is Info.
setLoggingSeverity :: Severity -> IO ()
setLoggingSeverity sev =
    modifyMVar_ loggingState $ const $ pure $ LogInternalState sev

-- | Colorizes "Text".
colorizer :: Severity -> Text -> Text
colorizer pr s =
    let (before, after) = table pr
    in toText before <> s <> toText after
  where
    -- | Defines pre- and post-printed characters for printing colorized text.
    table :: Severity -> (String, String)
    table severity = case severity of
        Error   -> (setColor Red     , reset)
        Debug   -> (setColor Green   , reset)
        Notice  -> (setColor Magenta , reset)
        Warning -> (setColor Yellow  , reset)
        Info    -> (setColor Blue    , reset)
      where
        setColor color = setSGRCode [SetColor Foreground Vivid color]
        reset = setSGRCode [Reset]

-- | Formats UTC time in next format: "%Y-%m-%d %H:%M:%S%Q %Z"
-- but %Q part show only in centiseconds (always 2 digits).
centiUtcTimeF :: UTCTime -> Text
centiUtcTimeF t =
    dateDashF t |+ " " +| hmsF t |++| centiSecondF t |+ " " +| tzNameF t |+ ""
  where
    centiSecondF = padRightF 3 '0' . T.take 3 . fmt . subsecondF

-- | Logs message with specified severity using logger name in context.
logMessage
    :: (MonadIO m)
    => Severity
    -> Text
    -> m ()
logMessage severity msg =
    liftIO $ withMVar loggingState $ \LogInternalState{..} -> do
        time <- liftIO $ getCurrentTime
        let text = format time
        when (severity >= lisMinSeverity) $ putStrLn text
  where
    format time = mconcat
        [ colorizer severity $ "[" <> show severity <> "]"
        , " ["
        , centiUtcTimeF time
        , "] "
        , msg
        ]

-- | Shortcuts for 'logMessage' to use according severity.
logDebug, logInfo, logNotice, logWarning, logError
    :: MonadIO m
    => Text -> m ()
logDebug   = logMessage Debug
logInfo    = logMessage Info
logNotice  = logMessage Notice
logWarning = logMessage Warning
logError   = logMessage Error
