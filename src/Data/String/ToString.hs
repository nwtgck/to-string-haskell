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

import qualified Data.ByteString.Lazy.Char8 as Char8LB
import qualified Data.ByteString as BS

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

instance ToString Char8LB.ByteString where
  toString = Char8LB.unpack

-- (All Show instances can be ToString)
instance ( Show a
         , TypeNeq a String
         , TypeNeq a Char8LB.ByteString
         ) => ToString a where
  toString = show 