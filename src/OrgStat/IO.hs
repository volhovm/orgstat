-- | Operations with files mostly.

module OrgStat.IO
       ( readOrgFile
       , readConfig
       ) where

import qualified Base             as Base
import qualified Data.ByteString  as BS
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Data.Yaml        (decodeEither)
import           System.Directory (doesFileExist)
import           System.FilePath  (takeBaseName, takeExtension)
import           System.Wlog      (WithLogger, logDebug)
import           Turtle           (procStrict)
import           Universum

import           OrgStat.Ast      (Org)
import           OrgStat.Config   (ConfigException (ConfigParseException), OrgStatConfig)
import           OrgStat.Parser   (runParser)
import           OrgStat.Util     (dropEnd)

data OrgIOException
    = OrgIOException Text
      -- ^ All exceptions related to reading files
    | ExternalException Text
      -- ^ Failed to run some external app (gpg)
    deriving (Typeable)

instance Base.Show OrgIOException where
    show (OrgIOException r)    = "IOException: " <> T.unpack r
    show (ExternalException r) = "ExternalException: " <> T.unpack r

instance Exception OrgIOException

-- | Attempts to read a file. If extension is ".gpg", asks a user to
-- decrypt it first. Returns a pair @(filename, content)@.
readOrgFile :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m (Text, Org)
readOrgFile fp = do
    logDebug $ "Reading org file " <> fpt
    unlessM (liftIO $ doesFileExist fp) $
        throwM $ OrgIOException $ "Org file " <> fpt <> " doesn't exist"
    (content, fname) <- case takeExtension fp of
        ".gpg" -> (,dropEnd 4 fp) <$> decryptGpg
        ".org" -> (,fp) <$> liftIO (TIO.readFile fp)
        _ -> throwM $ OrgIOException $
            "File " <> fpt <> " has unknown extension. Need to be .org or .org.gpg"
    let filename = T.pack $ takeBaseName fname
    logDebug $ "Parsing org file " <> fpt
    parsed <- runParser content
    pure (filename, parsed)
  where
    fpt = T.pack fp
    decryptGpg = do
        logDebug $ "Decrypting gpg file: " <> fpt
        (exCode, output) <- procStrict "gpg" ["--quiet", "--decrypt", fpt] empty
        case exCode of
            ExitSuccess   -> pass
            ExitFailure n ->
                throwM $ ExternalException $ "Failed with code " <> show n
        pure output

-- | Reads yaml config
readConfig :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m OrgStatConfig
readConfig fp = do
    logDebug $ "Reading config file from: " <> fpt
    unlessM (liftIO $ doesFileExist fp) $
        throwM $ OrgIOException $ "Config file " <> fpt <> " doesn't exist"
    res <- liftIO $ BS.readFile fp
    either (throwM . ConfigParseException . T.pack) pure $ decodeEither res
  where
    fpt = T.pack fp
