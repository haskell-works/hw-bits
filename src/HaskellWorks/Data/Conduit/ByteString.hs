module HaskellWorks.Data.Conduit.ByteString
  ( rechunk
  ) where

import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Conduit

rechunk :: Monad m => Int -> Conduit BS.ByteString m BS.ByteString
rechunk = rechunk' BS.empty

rechunk' :: Monad m => BS.ByteString -> Int -> Conduit BS.ByteString m BS.ByteString
rechunk' as n | BS.length as >= n = do
  yield (BS.take n as)
  rechunk' (BS.drop n as) n
rechunk' as n = do
  mbss <- slurp (n - BS.length as)
  case mbss of
    Just bss -> do
      let bs = BS.concat (as : bss)
      yield (BS.take n bs)
      rechunk' (BS.drop n bs) n
    Nothing -> unless (BS.null as) $ yield as

slurp :: Monad m => Int -> ConduitM BS.ByteString BS.ByteString m (Maybe [BS.ByteString])
slurp = go []
  where go rs n | n > 0 = do
          mbs <- await
          case mbs of
            Just bs -> go (bs : rs) (n - BS.length bs)
            Nothing -> if null rs then return Nothing else return (Just (reverse rs))
        go rs _ = if null rs then return Nothing else return (Just (reverse rs))
