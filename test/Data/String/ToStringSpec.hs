{-# LANGUAGE OverloadedStrings #-}

module Data.String.ToStringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.String
import qualified Data.ByteString.Lazy.Char8 as Char8LB
import qualified Data.Text as T

import Data.String.ToString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toString" $ do
    it "String => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: String)
      actual `shouldBe` expect

    it "Char8LB.ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: Char8LB.ByteString)
      actual `shouldBe` expect