module HaskellWorks.Data.Json.Succinct.Cursor where

import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Succinct.BalancedParens
import HaskellWorks.Data.Succinct.RankSelect.Simple

data JsonCursor v = JsonCursor
  { position :: Position
  , balancedParens :: SimpleBalancedParens v
  , interests :: Simple v
  }
