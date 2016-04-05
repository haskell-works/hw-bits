{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString                                  as BS
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           Foreign
import           GHC.Conc
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Time
import           System.IO.MMap

main :: IO ()
main = do
  bs <- BS.readFile "/Users/jky/Downloads/78mbs.json"
  !cursor <- measure (fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  print (jsonTokenAt (firstChild cursor))
  threadDelay 100000000
  print (jsonTokenAt cursor)
  return ()
