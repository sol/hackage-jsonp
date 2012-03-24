{-# LANGUAGE OverloadedStrings #-}
module Spec (main, spec) where

import           Test.Hspec.ShouldBe

import           Data.Aeson.Generic
import           Data.ByteString.Lazy.Char8 ()
import           Package
import           Main hiding (main)

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "encode" $ do
    it "converts a Package to JSON" $ do
      encode (Package "hspec" "0.9.2") `shouldBe` "{\"name\":\"hspec\",\"version\":\"0.9.2\"}"

  describe "parse" $ do
    it "parses a Package" $ do
      parse "Sun Mar 18 05:12:30 UTC 2012 SimonHengel hspec 0.9.2" `shouldBe` Right (Package "hspec" "0.9.2")
