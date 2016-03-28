{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.BasicGen
  ( module X
  ) where

import           Data.Typeable
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0Spec   as X hiding (spec)
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1Spec   as X hiding (spec)
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0Spec as X hiding (spec)
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1Spec as X hiding (spec)
import           Test.Hspec

genBinaryRankSelectSpec :: forall s. (Typeable s, BitRead s, Rank0 s, Rank1 s, Select0 s, Select1 s) => s -> Spec
genBinaryRankSelectSpec s = describe "Generically" $ do
  genBinaryRankSelectSpec s
