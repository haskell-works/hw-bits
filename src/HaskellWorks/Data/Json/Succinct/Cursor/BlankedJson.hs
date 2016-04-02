
module HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
  ( BlankedJson(..)
  , FromBlankedJson(..)
  ) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.ByteString
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString

newtype BlankedJson = BlankedJson [BS.ByteString] deriving (Eq, Show)

class FromBlankedJson a where
  fromBlankedJson :: BlankedJson -> a

instance FromByteString BlankedJson where
  fromByteString bs = BlankedJson (runListConduit blankJson (bsChunkBy 4096 bs))
