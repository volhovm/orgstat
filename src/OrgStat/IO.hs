-- | Operations with files mostly.

module OrgStat.IO (readOrgFile) where

import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           System.Directory (doesFileExist)
import           System.FilePath  (takeBaseName, takeExtension)
import           Turtle           (procStrict)
import           Universum

import           OrgStat.Ast      (Org)
import           OrgStat.Parser   (runParser)

data OrgIOException
    = OrgIOException Text
    | OrgGpgException Text
    deriving (Show,Typeable)

instance Exception OrgIOException

-- | Attempts to read a file. If extension is ".gpg", asks a user to
-- decrypt it first. Returns a pair @(filename, content)@.
readOrgFile :: (MonadIO m, MonadThrow m) => FilePath -> m (Text, Org)
readOrgFile fp = do
    unlessM (liftIO $ doesFileExist fp) $
        throwM $ OrgIOException $ "File " <> fpt <> " doesn't exist"
    (content, fname) <- case takeExtension fp of
        ".gpg" -> (,dropEnd 4 fp) <$> decryptGpg
        ".org" -> (,fp) <$> liftIO (TIO.readFile fp)
        _ -> throwM $ OrgIOException $
            "File " <> fpt <> " has unknown extension. Need to be .org or .org.gpg"
    let filename = T.pack $ takeBaseName fname
    parsed <- runParser content
    pure (filename, parsed)
  where
    fpt = T.pack fp
    dropEnd n xs = take (length xs - n) xs
    decryptGpg = do
        (exCode, output) <- procStrict "gpg" ["--quiet", "--decrypt", fpt] empty
        case exCode of
            ExitSuccess   -> pass
            ExitFailure n -> throwM $ OrgGpgException $ "Failed with code " <> show n
        pure output
