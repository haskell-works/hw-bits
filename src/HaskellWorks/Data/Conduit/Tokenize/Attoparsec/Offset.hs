{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Copyright: 2016 John Ky, 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Conduits for tokenizing streams.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module HaskellWorks.Data.Conduit.Tokenize.Attoparsec.Offset
    ( Offset (..)
    ) where

import           Control.Exception                                      (Exception)
import           Control.Monad.Trans.Resource                           (MonadThrow)
import qualified Data.Attoparsec.Types                                  as A
import qualified Data.ByteString                                        as B
import           Data.Conduit
import           Data.Int
import qualified Data.Text                                              as T
import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec.Internal
import           Prelude                                                hiding (lines)

data Offset = Offset
    { pos :: {-# UNPACK #-} !Int64
    }
    deriving (Eq, Ord)

instance Show Offset where
    show (Offset c) = show c

instance Exception (ParseError Offset)

instance Show (ParseDelta Offset) where
    show (ParseDelta s e) = show s ++ '-' : show e

instance AttoparsecState B.ByteString Offset where
    getState = B.foldl' f (Offset 0)
      where
        f (Offset c) _ = Offset (c + 1)
    modState x (Offset cols) = cols' `seq` Offset cols'
      where
        Offset dcols = getState x
        cols' = cols + dcols

instance AttoparsecState T.Text Offset where
    getState = T.foldl' f (Offset 0)
      where
        f (Offset c) _ = Offset (c + 1)
    modState x (Offset cols) =
        cols' `seq` Offset cols'
      where
        Offset dcols = getState x
        cols' = cols + dcols

{-# SPECIALIZE conduitParser
                  :: MonadThrow m
                  => Offset
                  -> A.Parser T.Text b
                  -> Conduit T.Text m (ParseDelta Offset, b) #-}

{-# SPECIALIZE conduitParser
                  :: MonadThrow m
                  => Offset
                  -> A.Parser B.ByteString b
                  -> Conduit B.ByteString m (ParseDelta Offset, b) #-}

{-# SPECIALIZE conduitParserEither
                  :: Monad m
                  => Offset
                  -> A.Parser T.Text b
                  -> Conduit T.Text m (Either (ParseError Offset) (ParseDelta Offset, b)) #-}

{-# SPECIALIZE conduitParserEither
                  :: Monad m
                  => Offset
                  -> A.Parser B.ByteString b
                  -> Conduit B.ByteString m (Either (ParseError Offset) (ParseDelta Offset, b)) #-}