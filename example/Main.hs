import           Data.Maybe
import qualified Data.Map as Map
import           Network.URI
import           Network.HTTP
import           Data.Aeson.Generic

url :: String
url = "http://www.typeful.net/~tbot/hackage/latest-package-versions.json"

-- | Return latest version of a package.
--
-- >>> getVersion "hspec"
-- Just "0.9.2"
getVersion :: String -> IO (Maybe String)
getVersion name = do
  r <- simpleHTTP request >>= getResponseBody
  return (decode r >>= Map.lookup name)
  where
    request = (mkRequest GET . fromJust . parseURI) url
