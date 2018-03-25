{-# LANGUAGE OverloadedStrings #-}

import Data.String.ToString
import Data.ByteString
import Data.Text

-- (This is an orignal type deriving `Show`)
data MyMaybe a = 
    MyJust a
  | MyNothing
  deriving Show

main :: IO ()
main = do
  let i :: Int
      i = 10
  print (toString i)
  -- => "10"

  let ch :: Char
      ch = 'd'
  print (toString ch)
  -- => "'d'"

  let myMay1 :: MyMaybe Double
      myMay1 = MyJust 1.89 
  print (toString myMay1)
  -- => "MyJust 1.89"

  let bs :: ByteString
      bs = "I'm a ByteString!"
  print (toString bs)
  -- => "I'm a ByteString!"

  let text :: Text
      text = "I'm a Text!"
  print (toString text)
  -- => "I'm a Text!"
