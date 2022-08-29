-- | Org-mode format parsing.

module OrgStat.Parser
       ( ParsingException (..)
       , parseOrg
       , runParser
       ) where

import Universum

import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.OrgMode.Parse as O
import qualified Data.OrgMode.Types as O
import qualified Data.Text as T
import Data.Time (LocalTime(..), TimeOfDay(..), fromGregorian, getZonedTime, zonedTimeToLocalTime)
import Data.Time.Calendar ()

import OrgStat.Ast (Clock(..), Org(..), orgTags, traverseTree, Title(..), Tag(..))

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data ParsingException = ParsingException Text
    deriving (Show, Typeable)

instance Exception ParsingException

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

parseOrg :: LocalTime -> [Text] -> A.Parser Org
parseOrg curTime todoKeywords = convertDocument <$> O.parseDocument todoKeywords
  where
    convertDocument :: O.Document -> Org
    convertDocument (O.Document textBefore headings) =
        let fileLvlTags = extractFileTags textBefore
            addTags t = ordNub $ (map Tag fileLvlTags) <> t
            o = Org { _orgTitle    = Title ""
                    , _orgTags     = []
                    , _orgClocks   = []
                    , _orgSubtrees = map convertHeading headings
                    }
        in o & traverseTree . orgTags %~ addTags

    convertHeading :: O.Headline -> Org
    convertHeading headline = Org
        { _orgTitle    = Title (O.title headline)
        , _orgTags     = map Tag $ O.tags headline
        , _orgClocks   = getClocks $ O.section headline
        , _orgSubtrees = map convertHeading $ O.subHeadlines headline
        }

    mapEither :: (a -> Either e b) -> ([a] -> [b])
    mapEither f xs = rights $ map f xs

    getClocks :: O.Section -> [Clock]
    getClocks section =
        mapMaybe convertClock $ concat
          [ O.sectionClocks section
          , O.unLogbook (O.sectionLogbook section)
          , mapEither (A.parseOnly O.parseClock) $ concat
            [ concatMap lines $ map O.contents $ O.sectionDrawers section
            , lines $ O.sectionParagraph section
            ]
          ]

    -- convert clocks from orgmode-parse format, returns Nothing for clocks
    -- without end time or time-of-day
    convertClock :: O.Clock -> Maybe Clock
    convertClock (O.Clock (Just (O.Timestamp start _active (Just end)), _duration)) =
        Clock <$> convertDateTime start <*> convertDateTime end
    convertClock (O.Clock (Just (O.Timestamp start _active Nothing), _duration)) =
        Clock <$> convertDateTime start <*> pure curTime
    convertClock _                                                 = Nothing

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

    extractFileTags (T.lines -> inputLines) =
        let prfx = "#+FILETAGS: "
            matching =
                map (T.drop (length prfx)) $
                (filter (prfx `T.isPrefixOf`) inputLines)
            toTags (T.strip -> line) =
                let parts = filter (not . T.null) $ T.splitOn ":" line
                    -- Correct tag shouldn't contain spaces inside
                    correctPart p = not $ T.any isSpace p
                in if all correctPart parts then parts else []
        in ordNub $ concatMap toTags matching

-- Throw parsing exception if it can't be parsed (use Control.Monad.Catch#throwM)
runParser :: (MonadIO m, MonadThrow m) => [Text] -> Text -> m Org
runParser todoKeywords t = do
    localTime <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
    case A.parseOnly (parseOrg localTime todoKeywords) t of
      Left err  -> throwM $ ParsingException $ T.pack err
      Right res -> pure res
