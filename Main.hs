module Main where

import           Package hiding (name, version)
import qualified Package

parse :: String -> Either String Package
parse input = case (reverse . words) input of
  version : name : _ -> Right (Package name version)
  _                  -> Left ("invalid input: " ++ show input)
