module HaskellWorks.Data.Bits.BitPatterns
  ( BitPatterns(..)
  ) where

import Data.Word

class BitPatterns a where
  -- | Bit-wise value of the given type with all bits set to zero
  all0s :: a
  -- | Bit-wise value of the given type with all bits set to one
  all1s :: a

instance BitPatterns Word8 where
  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xff
  {-# INLINE all1s #-}

instance BitPatterns Word16 where
  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffff
  {-# INLINE all1s #-}

instance BitPatterns Word32 where
  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffffffff
  {-# INLINE all1s #-}

instance BitPatterns Word64 where
  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffffffffffffffff
  {-# INLINE all1s #-}
