module Parse where

import           Data.Map   (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as L

-- | Parse package name and version from a Hackage upload log line.
parse :: L.ByteString -> (L.ByteString, L.ByteString)
parse input = case (reverse . L.words) input of
  version : name : _ -> (name, version)
  _                  -> error ("invalid input: " ++ show input)

-- | Parse Hackage upload log, and store latest package versions in a map.
parseMany :: L.ByteString -> Map L.ByteString L.ByteString
parseMany input = foldr f Map.empty (L.lines input)
  where
    f s = Map.alter g name
      where
        g (Just old) | version < old = Just old
        g _                          = Just version
        (name, version) = parse s

