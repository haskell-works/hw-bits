
module HaskellWorks.Data.Conduit.List
  ( runListConduit
  ) where

import           Data.Conduit
import           Data.Conduit.List as CL
import           Prelude           as P

runListConduit :: [i] -> Conduit i [] o -> [o]
runListConduit is c = P.concat $ sourceList is =$ c $$ consume
