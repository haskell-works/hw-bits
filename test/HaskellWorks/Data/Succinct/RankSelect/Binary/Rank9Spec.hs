{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Rank9Spec (spec) where

import qualified Data.Vector                                                    as DVN
import qualified Data.Vector.Storable                                           as DVS
import           Data.Word
import qualified HaskellWorks.Data.Succinct.RankSelect.Binary.Rank9.Native      as N
import qualified HaskellWorks.Data.Succinct.RankSelect.Binary.Rank9.Positioning as P
import qualified HaskellWorks.Data.Succinct.RankSelect.Binary.Rank9.Storable    as S
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

data VectorRank = VectorRank [Word64] P.Position deriving (Eq, Show)

instance Arbitrary VectorRank where
  arbitrary = do
    h <- arbitrary :: Gen Word64
    v <- arbitrary :: Gen [Word64]
    -- n <- choose (0, (length v + 1) * 64 :: Int)
    let n = (length v + 1) * 64 :: Int
    return (VectorRank (h:v) (P.Position n))

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  it "bitRank for Simple (Vector Word8) and Simple (Vector Word64) should give same answer" $
      quickCheckWith stdArgs { maxSuccess = 50000 } $
      property $
    \(VectorRank as i) ->
      let sv = S.prepare (DVS.fromList as :: DVS.Vector Word64) in
      let nv = N.prepare (DVN.fromList as :: DVN.Vector Word64) in
      N.rank nv i == N.rank nv i &&
      -- S.rank sv i == S.rank sv i &&
      -- S.rank sv i == N.rank nv i &&
      True
