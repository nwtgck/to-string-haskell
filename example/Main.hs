{-# LANGUAGE OverloadedStrings #-}

import Data.String.ToString
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- (This is an orignal type deriving `Show`)
data MyMaybe a = 
    MyJust a
  | MyNothing
  deriving Show

main :: IO ()
main = do
  let i :: Int
      i = 10
  putStrLn (toString i)
  -- => 10

  let ch :: Char
      ch = 'd'
  putStrLn (toString ch)
  -- => 'd'

  let myMay1 :: MyMaybe Double
      myMay1 = MyJust 1.89 
  putStrLn (toString myMay1)
  -- => MyJust 1.89

  let bs :: BS.ByteString
      bs = "I'm a ByteString!"
  putStrLn (toString bs)
  -- => I'm a ByteString!

  let text :: T.Text
      text = "I'm a Text!"
  putStrLn (toString text)
  -- => I'm a Text!
