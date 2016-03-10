{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
    ( Select0(..)
    ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.VectorLike

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

class Select0 v where
  select0 :: v -> Count -> Count

instance Select0 (DVS.Vector Word8) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc  -> select0 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + 8)
  {-# INLINABLE select0 #-}

-- TODO: Implement NOT in terms of select for word-16
instance Select0 Word8 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word16 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word32 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word64 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 [Bool] where
  select0 = go 0
    where go r _ 0 = r
          go r (False:bs) c = go (r + 1) bs (c - 1)
          go r (True:bs)  c = go (r + 1) bs  c
          go _ []         _ = error "Out of range"
  {-# INLINABLE select0 #-}
