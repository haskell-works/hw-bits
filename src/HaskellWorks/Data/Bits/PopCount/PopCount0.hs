{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.PopCount.PopCount0
    ( PopCount0(..)
    ) where

import qualified Data.Vector                               as DV
import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Bits.Types.Broadword
import           HaskellWorks.Data.Bits.Types.Builtin
import           HaskellWorks.Data.Positioning
import           Prelude                                   as P

-- | The number of zero bits in the value.
class PopCount0 v where
  popCount0 :: v -> Count

instance PopCount0 Bool where
  popCount0 True  = 0
  popCount0 False = 1
  {-# INLINE popCount0 #-}

instance PopCount0 Word8 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 Word16 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 Word32 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 Word64 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word8) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word16) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word32) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word64) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word8) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word16) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word32) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word64) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 a => PopCount0 [a] where
  popCount0 = P.sum . fmap popCount0
  {-# INLINE popCount0 #-}

instance PopCount0 a => PopCount0 (DV.Vector a) where
  popCount0 = DV.foldl (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance (DVS.Storable a, PopCount0 a) => PopCount0 (DVS.Vector a) where
  popCount0 = DVS.foldl (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}
