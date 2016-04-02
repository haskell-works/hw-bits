module HaskellWorks.Data.ByteString
  ( chunkedBy
  ) where

import qualified Data.ByteString as BS

chunkedBy :: Int -> BS.ByteString -> [BS.ByteString]
chunkedBy n bs = if BS.length bs == 0
  then []
  else case BS.splitAt n bs of
    (as, zs) -> as : chunkedBy n zs
