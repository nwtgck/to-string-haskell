# to-string
[![Build Status](https://travis-ci.com/nwtgck/to-string-haskell.svg?token=TuxNpqznwwyy7hyJwBVm&branch=master)](https://travis-ci.com/nwtgck/to-string-haskell)

A `toString` converter for String-like types and any type which is `Show a => a`.

## Usages

```hs
toString 89
-- => "89"
```

```hs
toString "hello"
-- => "hello"
```

```hs
{-# LANGUAGE OverloadedStrings #-}

toString ("I'm ByteString" :: BS.ByteString)
-- => "I'm ByteString"
```

```hs
{-# LANGUAGE OverloadedStrings #-}

toString ("I'm Text" :: T.Text)
-- => "I'm Text"
```

## Supported String-like types


* `Data.ByteString.Char8.ByteString`
* `Data.ByteString.Lazy.Char8.ByteString`
* `Data.ByteString.Short.ShortByteString`
* `Data.ByteString.UTF8.ByteString`
* `Data.ByteString.Lazy.UTF8.ByteString`
* `Data.Text.Text`
* `Data.Text.Lazy.Text`

### Any `Show a => a`

Any `Show a => a` type is also an instance of type class `ToString`.  
So `Int`, `Double`, `Maybe a` ... which are `Show` instances are also instances of `ToString`

## Executable example

```hs
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
```