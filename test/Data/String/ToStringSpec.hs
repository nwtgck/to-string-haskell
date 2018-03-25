{-# LANGUAGE OverloadedStrings #-}

module Data.String.ToStringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.String
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy.Char8
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
    
    it "BS.Char8.ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Char8.ByteString)
      actual `shouldBe` expect

    it "BS.Lazy.Char8.ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Lazy.Char8.ByteString)
      actual `shouldBe` expect
    
    it "T.Text => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: T.Text)
      actual `shouldBe` expect