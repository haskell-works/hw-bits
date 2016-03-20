-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Tree Cursor
module HaskellWorks.Data.Succinct
    ( TreeCursor(..)
    ) where

import           HaskellWorks.Data.Positioning

class TreeCursor k where
  firstChild :: k -> k
  nextSibling :: k -> k
  parent :: k -> k
  depth :: k -> Count
  subtreeSize :: k -> Count
