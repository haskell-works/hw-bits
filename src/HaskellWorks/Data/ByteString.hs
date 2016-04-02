module HaskellWorks.Data.ByteString
  ( bsChunkBy
  ) where

import qualified Data.ByteString as BS

bsChunkBy :: Int -> BS.ByteString -> [BS.ByteString]
bsChunkBy n bs = if BS.length bs == 0
  then []
  else case BS.splitAt n bs of
    (as, zs) -> as : bsChunkBy n zs
