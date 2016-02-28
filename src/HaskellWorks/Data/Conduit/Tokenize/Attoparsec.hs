{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- |
-- Copyright: 2016 John Ky, 2011 Michael Snoyman, 2010 John Millikin
-- License: MIT
--
-- Conduits for tokenizing streams.
--
-- This code was taken from attoparsec-enumerator and adapted for conduits.
module HaskellWorks.Data.Conduit.Tokenize.Attoparsec
    ( -- * Sink
      sinkParser
    , sinkParserEither
      -- * Conduit
    , conduitParser
    , conduitParserEither

      -- * Types
    , ParseError (..)
    , ParseDelta (..)
      -- * Classes
    , AttoparsecInput(..)
    , AttoparsecState(..)
    ) where

import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec.Internal
