
module HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString

newtype BlankedJson = BlankedJson [BS.ByteString] deriving (Eq, Show)

instance FromByteString BlankedJson where
  fromByteString bs = BlankedJson (runListConduit blankJson [bs])
