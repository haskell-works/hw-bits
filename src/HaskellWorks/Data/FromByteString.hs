module HaskellWorks.Data.FromByteString
  ( FromByteString(..)
  ) where

import           Data.ByteString.Internal

class FromByteString a where
  fromByteString :: ByteString -> a
