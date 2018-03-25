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
import qualified Data.Text as T

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

instance ToString T.Text where
  toString = T.unpack

-- (All Show instances can be ToString)
instance ( Show a
         , TypeNeq a String
         , TypeNeq a BS.Char8.ByteString         
         , TypeNeq a BS.Lazy.Char8.ByteString
         , TypeNeq a T.Text         
         ) => ToString a where
  toString = show 