{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE IncoherentInstances #-}

-- (Comment out: These pragmas can be used in the future)
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}

module Data.String.ToString where

import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy.Char8
import qualified Data.ByteString.Short as BS.Short

import qualified Data.Text as T
import qualified Data.Text.Lazy as T.Lazy

-- Type equality
-- (from: https://stackoverflow.com/a/25119516/2885946)
data TrueType
data FalseType
type family TypeEqF a b where
  TypeEqF a a = TrueType
  TypeEqF a b = FalseType
type TypeNeq a b = TypeEqF a b ~ FalseType


class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance ToString BS.Char8.ByteString where
  toString = BS.Char8.unpack

instance ToString BS.Lazy.Char8.ByteString where
  toString = BS.Lazy.Char8.unpack

instance ToString BS.Short.ShortByteString where
  toString = toString . BS.Short.fromShort

instance ToString T.Text where
  toString = T.unpack

instance ToString T.Lazy.Text where
  toString = T.Lazy.unpack

-- (All Show instances can be ToString)
instance ( Show a
         , TypeNeq a String
         , TypeNeq a BS.Char8.ByteString         
         , TypeNeq a BS.Lazy.Char8.ByteString
         , TypeNeq a BS.Short.ShortByteString
         , TypeNeq a T.Text
         , TypeNeq a T.Lazy.Text
         ) => ToString a where
  toString = show 