module HaskellWorks.Data.Conduit.Json.Words where

import           Data.Char
import           Data.Word

wBackslash :: Word8
wBackslash = fromIntegral (ord '\\')

wDoubleQuote :: Word8
wDoubleQuote = fromIntegral (ord '"')

wUnderscore :: Word8
wUnderscore = fromIntegral (ord '_')

wSpace :: Word8
wSpace = fromIntegral (ord ' ')

wOpenParen :: Word8
wOpenParen = fromIntegral (ord '(')

wCloseParen :: Word8
wCloseParen = fromIntegral (ord ')')

wPlus :: Word8
wPlus = fromIntegral (ord '+')

we :: Word8
we = fromIntegral (ord 'e')

wE :: Word8
wE = fromIntegral (ord 'E')

wDot :: Word8
wDot = fromIntegral (ord '.')

wMinus :: Word8
wMinus = fromIntegral (ord '-')

w0 :: Word8
w0 = fromIntegral (ord '0')

w1 :: Word8
w1 = fromIntegral (ord '1')

w9 :: Word8
w9 = fromIntegral (ord '9')

isLeadingDigit :: Word8 -> Bool
isLeadingDigit w = w == wMinus || (w >= w0 && w <= w9)

isTrailingDigit :: Word8 -> Bool
isTrailingDigit w = w == wPlus || w == wMinus || (w >= w0 && w <= w9) || w == wDot || w == wE || w == we
