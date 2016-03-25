module HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
    ( Poppy512(..)
    , Rank1(..)
    , makePoppy512
    ) where

import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Vector.VectorLike
import           Prelude                                                  as P

data Poppy512 = Poppy512
  { poppy512Bits  :: DVS.Vector Word64
  , poppy512Index :: DVS.Vector Word64
  } deriving Show

makePoppy512 :: DVS.Vector Word64 -> Poppy512
makePoppy512 v = Poppy512
  { poppy512Bits  = v
  , poppy512Index = DVS.constructN ((DVS.length v `div` 8) + 1) gen512Index
  }
  where gen512Index :: DVS.Vector Word64 -> Word64
        gen512Index u = getCount (popCount1 (DVS.take (DVS.length u * 8) u))

instance Rank1 Poppy512 where
  rank1 (Poppy512 v i) p =
    Count (i !!! toPosition (p `div` 512)) + rank1 (DVS.drop (fromIntegral p `div` 512) v) (p `mod` 512)
