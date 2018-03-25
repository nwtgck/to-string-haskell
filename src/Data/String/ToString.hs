{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE IncoherentInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

module Data.String.ToString where

import qualified Data.ByteString.Lazy.Char8 as Char8LB
import qualified Data.ByteString as BS

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance ToString Char8LB.ByteString where
  toString = Char8LB.unpack

instance Show a => ToString a where
  toString = show 