# to-string
[![Build Status](https://travis-ci.com/nwtgck/to-string-haskell.svg?token=TuxNpqznwwyy7hyJwBVm&branch=master)](https://travis-ci.com/nwtgck/to-string-haskell)

A `toString` converter for String-like types and any `Show a => a` type.

## Usages

```hs
toString 89 == "89"
```

```hs
toString "hello" == "hello"
```

```hs
{-# LANGUAGE OverloadedStrings #-}
toString ("I'm a ByteString" :: BS.ByteString) == ("I'm a ByteString" :: String)
```

```hs
{-# LANGUAGE OverloadedStrings #-}

toString ("I'm a Text" :: T.Text) == ("I'm a Text" :: String)
```

## Supported String-like types

* `String`
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
```

### Output

```txt
10
'd'
MyJust 1.89
I'm a ByteString!
I'm a Text!
```