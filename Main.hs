module Main where

import           Data.Map   (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LB

import           Data.Aeson.Generic

parse :: String -> (String, String)
parse input = case (reverse . words) input of
  version : name : _ -> (name, version)
  _                  -> error ("invalid input: " ++ show input)

parseMany :: String -> Map String String
parseMany input = foldr f Map.empty (lines input)
  where
    f s = Map.alter g name
      where
        g (Just old) | version < old = Just old
        g _                          = Just version
        (name, version) = parse s

main :: IO ()
main = do
  input <- readFile "log"
  let packages = parseMany input
  LB.writeFile "log.json" (encode packages)
