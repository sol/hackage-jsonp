{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Run (main) where

import           Control.Exception
import           Data.Foldable (forM_)
import           Data.Maybe
import           System.IO (hPutStrLn, stderr, withFile, IOMode(..))
import           System.Time (getClockTime)
import           Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString (ByteString)
import           Control.Monad (when)
import           Control.Monad.IO.Class
import qualified Data.Aeson
import qualified Data.Aeson.Generic
import           Data.Conduit
import           Network.HTTP.Types
import           Network.HTTP.Conduit
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import           System.FilePath
import           System.Directory
import           System.Posix.Files
import           Distribution.Package
import           Distribution.Text
import           Distribution.PackageDescription.Parse

import           Parse
import           CabalJSON ()

-- | Output file.
latestVersionsFileName :: FilePath
latestVersionsFileName = "latest-package-versions"

pkgDirectoryName :: FilePath
pkgDirectoryName = "packages"

-- | URL to Hackage upload log.
uploadLogUrl :: String
uploadLogUrl = "http://hackage.haskell.org/packages/archive/log"

pkgDescriptionsURL :: String
pkgDescriptionsURL = "http://hackage.haskell.org/packages/archive/00-index.tar.gz"

writeJsonAndJsonp :: L.ByteString -> FilePath -> L.ByteString -> IO ()
writeJsonAndJsonp callback fileName json = do 
  withFile (fileName ++ ".jsonp") WriteMode $ \h -> do
    L.hPut h callback
    L.hPut h "("
    L.hPut h json
    L.hPut h ");"
  withFile (fileName ++ ".json") WriteMode $ \h -> do
    L.hPut h json

-- | Parse given data, and write it as both JSON and JSONP.
writeJSONP :: L.ByteString -> IO ()
writeJSONP updatefile = do
  logInfo "writing version file"
  writeJsonAndJsonp "hackagePackageVersionsCallback" latestVersionsFileName .
    Data.Aeson.Generic.encode . parseMany $ updatefile
  logInfo "writing version file done"

writePackagesJSON :: L.ByteString -> IO ()
writePackagesJSON tarfile = do
    logInfo "writing packages files"
    createDirectoryIfMissing False pkgDirectoryName
    foldEntriesM_ writePackageJSON (logError.show) . Tar.read . GZip.decompress $ tarfile
    logInfo "writing packages files done"

updateLink :: String -> PackageIdentifier -> IO ()
updateLink suffix pkg = do
    exists <- doesFileExist linkfile
    if exists
     then do
        target <- readSymbolicLink linkfile
        let pkg' = fromJust . simpleParse . init . (reverse . dropWhile (/= '.') . reverse) $ target
        when (pkgVersion pkg' < pkgVersion pkg) $ do
           removeFile linkfile
           createSymbolicLink thisfileName linkfile
     else do
        createSymbolicLink thisfileName linkfile
  where linkfile = pkgDirectoryName </> display (packageName pkg) </> display (packageName pkg) ++ "." ++ suffix
        thisfileName = display pkg ++ "." ++ suffix

writePackageJSON :: Tar.Entry -> IO ()
writePackageJSON e | "preferred-versions" <- Tar.entryPath e = return ()
writePackageJSON e | Tar.NormalFile rawDesc _ <- Tar.entryContent e = do
    case parsePackageDescription (L.unpack rawDesc) of
        ParseFailed err -> do
            logError $ "Parse error in " ++ Tar.entryPath e ++ ": " ++ show err           
        ParseOk _ desc -> do
            when (null (display (packageId desc))) $ 
                logError $ "Empty package name in " ++ Tar.entryPath e

            let dir = pkgDirectoryName </> display (packageName (packageId desc))
            let fileName = dir </> display (packageId desc)
            createDirectoryIfMissing False dir
            updateLink "json" (packageId desc)
            updateLink "jsonp" (packageId desc)
            writeJsonAndJsonp "hackageDataCallback" fileName (Data.Aeson.encode desc)
writePackageJSON _ = return ()

foldEntriesM_ :: Monad m => (Tar.Entry -> m ()) -> (e -> m ()) -> Tar.Entries e -> m ()
foldEntriesM_ f g = Tar.foldEntries ((>>) . f) (return ()) g


-- |
-- Download the Hackage upload log, parse it, and write latest package versions
-- into a JSONP file.
--
-- Repeatedly check if the upload log has changed, and if so, regenerate the
-- JSONP file.
main :: IO ()
main = go ("","")
  where
    go (etag1, etag2) = do
      (e1, r) <- tryUpdate uploadLogUrl etag1
      forM_ r writeJSONP

      (e2, t) <- tryUpdate pkgDescriptionsURL etag2
      forM_ t writePackagesJSON

      -- sleep for 60 seconds
      threadDelay 60000000
      go (e1,e2)

    tryUpdate url etag = do
      update url etag `catches` [
        -- Re-throw AsyncException, otherwise execution will not terminate on
        -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
        -- UserInterrupt) because all of them indicate severe conditions and
        -- should not occur during normal operation.
        Handler (\e -> throw (e :: AsyncException)),

        Handler (\e -> do
          logError $ show (e :: SomeException)
          return (etag, Nothing)
          )
        ]

    update :: String -> ByteString -> IO (ByteString, Maybe L.ByteString)
    update url etag = withManager $ \manager -> do
      e <- getEtag url manager
      if etag == e
        then do
          logInfo "nothing changed"
          return (e, Nothing)
        else do
          logInfo $ "updating " ++ url
          r <- getLog url manager
          logInfo "updating done"
          (return . fmap Just) r

-- | Write a log message to stderr.
logInfo :: MonadIO m => String -> m ()
logInfo msg = liftIO $ do
  t <- getClockTime
  hPutStrLn stderr (show t ++ ": " ++ msg)

-- | Write a log message to stderr.
logError :: MonadIO m => String -> m ()
logError = logInfo . ("ERROR: " ++)

-- | Get etag of an URL
getEtag :: String -> Manager -> ResourceT IO ByteString
getEtag url manager = do
  Response _ _ header _ <- httpLbs (logRequest url) {method = "HEAD"} manager
  return (etagHeader header)

-- | Get an URL
getLog :: String -> Manager -> ResourceT IO (ByteString, L.ByteString)
getLog url manager = do
  Response _ _ header body <- httpLbs (logRequest url) manager
  return (etagHeader header, body)

-- | Get etag from headers.
--
-- Fail with `error`, if there is no etag.
etagHeader :: ResponseHeaders -> ByteString
etagHeader = maybe (error "etagHeader: no etag!") id . lookup "etag"

logRequest :: String -> Request t
logRequest url = req {redirectCount = 0}
  where
    req = maybe (error $ "logRequest: invalid URL " ++ show url) id (parseUrl url)
