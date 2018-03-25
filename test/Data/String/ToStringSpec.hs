{-# LANGUAGE OverloadedStrings #-}

module Data.String.ToStringSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.String
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy.Char8
import qualified Data.ByteString.Short as BS.Short

import qualified Data.ByteString.UTF8 as BS.UTF8
import qualified Data.ByteString.Lazy.UTF8 as BS.Lazy.UTF8

import qualified Data.Text as T
import qualified Data.Text.Lazy as T.Lazy

import Data.String.ToString

main :: IO ()
main = hspec spec

data MyType =
    MyData1 Int String
  | MyData2 Double
  | MyData3 Char MyType
  deriving Show

spec :: Spec
spec = do
  describe "toString" $ do

    it "Int => String" $ do
      let expect = show 1818
          actual = toString (1818 :: Int)
      actual `shouldBe` expect

    it "Char => String" $ do
      let expect = show 'f'
          actual = toString ('f' :: Char)
      actual `shouldBe` expect
    
    it "Float => String" $ do
      let expect = show 5.28
          actual = toString (5.28 :: Float)
      actual `shouldBe` expect

    it "MyType => String" $ do
      let mydata1 = MyData1 3366 "sample text1"
      toString mydata1 `shouldBe` show mydata1

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
  
    it "BS.Short.ShortByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Short.ShortByteString)
      actual `shouldBe` expect
      
    it "BS.UTF8.ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.UTF8.ByteString)
      actual `shouldBe` expect
    
    it "BS.Lazy.UTF8.ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Lazy.UTF8.ByteString)
      actual `shouldBe` expect

    it "T.Text => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: T.Text)
      actual `shouldBe` expect
    
    it "T.Lazy.Text => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: T.Lazy.Text)
      actual `shouldBe` expect