{-# LANGUAGE OverloadedStrings #-}
module Run (main) where

import           Control.Exception
import           Data.Foldable (forM_)
import           System.IO (hPutStrLn, stderr, withFile, IOMode(..))
import           System.Time (getClockTime)
import           Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString (ByteString)
import           Control.Monad.IO.Class
import           Data.Aeson.Generic
import           Data.Conduit
import           Network.HTTP.Types
import           Network.HTTP.Conduit

import           Parse

-- | Output file.
fileName :: FilePath
fileName = "latest-package-versions"

-- | URL to Hackage upload log.
uploadLogUrl :: String
uploadLogUrl = "http://hackage.haskell.org/packages/archive/log"

-- | Parse given data, and write it as both JSON and JSONP.
writeJSONP :: L.ByteString -> IO ()
writeJSONP input = do
  withFile (fileName ++ ".jsonp") WriteMode $ \h -> do
    L.hPut h "hackagePackageVersionsCallback("
    L.hPut h json
    L.hPut h ");"
  withFile (fileName ++ ".json") WriteMode $ \h -> do
    L.hPut h json
  where
    json = (encode . parseMany) input

-- |
-- Download the Hackage upload log, parse it, and write latest package versions
-- into a JSONP file.
--
-- Repeatedly check if the upload log has changed, and if so, regenerate the
-- JSONP file.
main :: IO ()
main = go ""
  where
    go etag = do
      (e, r) <- tryUpdate etag
      forM_ r writeJSONP

      -- sleep for 60 seconds
      threadDelay 60000000
      go e

    tryUpdate etag = do
      update etag `catches` [
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

    update :: ByteString -> IO (ByteString, Maybe L.ByteString)
    update etag = withManager $ \manager -> do
      e <- getEtag manager
      if etag == e
        then do
          logInfo "nothing changed"
          return (e, Nothing)
        else do
          logInfo "updating"
          r <- getLog manager
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

-- | Get etag of Hackage upload log.
getEtag :: Manager -> ResourceT IO ByteString
getEtag manager = do
  Response _ _ header _ <- httpLbs logRequest {method = "HEAD"} manager
  return (etagHeader header)

-- | Get Hackage upload log.
getLog :: Manager -> ResourceT IO (ByteString, L.ByteString)
getLog manager = do
  Response _ _ header body <- httpLbs logRequest manager
  return (etagHeader header, body)

-- | Get etag from headers.
--
-- Fail with `error`, if there is no etag.
etagHeader :: ResponseHeaders -> ByteString
etagHeader = maybe (error "etagHeader: no etag!") id . lookup "etag"

logRequest :: Request t
logRequest = req {redirectCount = 0}
  where
    req = maybe (error $ "logRequest: invalid URL " ++ show uploadLogUrl) id (parseUrl uploadLogUrl)
