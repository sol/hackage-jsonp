{-# LANGUAGE OverloadedStrings #-}
module Spec (main, spec) where

import           Test.Hspec.ShouldBe

import           Data.Aeson.Generic
import           Data.ByteString.Lazy.Char8 ()
import           Package

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "encode" $ do
    it "converts a Package to JSON" $ do
      encode (Package "hspec" "0.9.2") `shouldBe` "{\"name\":\"hspec\",\"version\":\"0.9.2\"}"
