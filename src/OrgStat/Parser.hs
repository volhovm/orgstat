-- | Org-mode format parsing.

module OrgStat.Parser
       ( ParsingException (..)
       , parseOrg
       , runParser
       ) where

import Universum

import Control.Exception (Exception)
import qualified Data.Attoparsec.Text as A
import qualified Data.OrgMode.Parse as O
import qualified Data.OrgMode.Types as O
import qualified Data.Text as T
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import Data.Time.Calendar ()

import OrgStat.Ast (Clock (..), Org (..))

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data ParsingException =
    ParsingException Text
    deriving (Show, Typeable)

instance Exception ParsingException

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

parseOrg :: [Text] -> A.Parser Org
parseOrg todoKeywords = convertDocument <$> O.parseDocument todoKeywords
  where
    convertDocument :: O.Document -> Org
    convertDocument (O.Document _ headings) = Org
        { _orgTitle    = ""
        , _orgTags     = []
        , _orgClocks   = []
        , _orgSubtrees = map convertHeading headings
        }

    convertHeading :: O.Headline -> Org
    convertHeading headline = Org
        { _orgTitle    = O.title headline
        , _orgTags     = O.tags headline
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

-- Throw parsing exception if it can't be parsed (use Control.Monad.Catch#throwM)
runParser :: (MonadThrow m) => [Text] -> Text -> m Org
runParser todoKeywords t =
    case A.parseOnly (parseOrg todoKeywords) t of
      Left err  -> throwM $ ParsingException $ T.pack err
      Right res -> pure res
