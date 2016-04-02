
module HaskellWorks.Data.Conduit.List
  ( runListConduit
  ) where

import           Data.Conduit
import           Data.Conduit.List as CL
import           Prelude           as P

runListConduit :: Conduit i [] o -> [i] -> [o]
runListConduit c is = P.concat $ sourceList is =$ c $$ consume
