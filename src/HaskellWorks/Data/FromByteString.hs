module HaskellWorks.Data.FromByteString
  ( FromByteString(..)
  ) where

import           Data.ByteString.Internal

-- | Class for byte-string-like datastructures
class FromByteString a where
  -- | Convert a byte string to a value of type @a
  fromByteString :: ByteString -> a
