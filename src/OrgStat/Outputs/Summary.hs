-- | Summary output type.

module OrgStat.Outputs.Summary
       ( genSummaryOutput
       ) where

import           Universum

import           Control.Lens                     (views)
import qualified Data.Attoparsec.ByteString.Char8 as A

import           OrgStat.Ast                      (clockDuration, orgClocks, traverseTree)
import           OrgStat.Config                   (confReports, crName)
import           OrgStat.Helpers                  (resolveReport)
import           OrgStat.Outputs.Types            (SummaryOutput (..), SummaryParams (..))
import           OrgStat.WorkMonad                (WorkM, wcConfig)


-- | Tokenizes summary template string.
data InputToken
    = ReportTemplate Text -- ^ Valid template name surrounded by two '%'
    | OtherInfo Text          -- ^ Anything in between
    deriving Show

tokenize :: [Text] -> Text -> [InputToken]
tokenize keywords (encodeUtf8 -> input) =
    case A.parseOnly toplvl input of
      Left err  -> error $ fromString err
      Right res -> res
  where
    keyword = A.try $ do
        void $ A.char '%'
        between <- fromString <$> some (A.satisfy (/= '%'))
        void $ A.char '%'
        guard $ between `elem` keywords
        pure $ ReportTemplate between
    randomtext = do
        h <- A.anyChar
        t <- fromString <$> many (A.satisfy (/= '%'))
        pure $ OtherInfo $ fromString $ h:t
    toplvl = many (keyword <|> randomtext)

-- | Generates summary using provided params.
genSummaryOutput :: SummaryParams -> WorkM SummaryOutput
genSummaryOutput SummaryParams{..} = do
    declaredReports <- views wcConfig $ map crName . confReports
    let tokens = tokenize declaredReports spTemplate
    res <- fmap mconcat $ forM tokens $ \case
        OtherInfo t -> pure t
        ReportTemplate reportName -> do
            report <- resolveReport reportName
            let allClocks = concat $ report ^.. traverseTree . orgClocks
            let totalTimeMin :: Integer
                totalTimeMin = round $ (/ 60) $ toRational $ sum $ map clockDuration allClocks
            let hours = totalTimeMin `div` 60
            let minutes = totalTimeMin `mod` 60
            pure $ show hours <> ":" <> show minutes
    pure $ SummaryOutput res
