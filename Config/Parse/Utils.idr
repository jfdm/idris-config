-- --------------------------------------------------------------- [ Utils.idr ]
-- Description : Utility parsing functions.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Utils

import Control.Monad.Identity
import Data.Fin

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

%access public

-- ------------------------------------------------------------------- [ Stuff ]
-- These should be merged into Lightyear

eol : Parser ()
eol = char '\n'

anyChar : Parser Char
anyChar = satisfy (const True)

word : Parser String
word = map pack (some $ satisfy isAlphaNum) <?> "Word"

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

manyTill : Monad m => ParserT m String a
                   -> ParserT m String b
                   -> ParserT m String (List a)
manyTill p end = scan
  where
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}

comment : Monad m => String -> ParserT m String ()
comment str = do
    skip $ string str $> (manyTill (satisfy (const True)) (satisfy (== '\n')))
   <?> unwords ["Comment with char", str]

-- ------------------------------------------------------------- [ Hex Numbers ]
-- Borrowed from Lightyear JSON Examples
hex : Parser Int
hex = do
  c <- map (ord . toUpper) $ satisfy isHexDigit
  pure $ if c >= ord '0' && c <= ord '9'
           then c - ord '0'
           else 10 + c - ord 'A'

-- Borrowed from Lightyear JSON Examples
hexQuad : Parser Int
hexQuad = do
  a <- hex
  b <- hex
  c <- hex
  d <- hex
  pure $ a * 4096 + b * 256 + c * 16 + d

--Borrowed from Lightyear JSON Examples.
specialChar : Parser Char
specialChar = do
  c <- satisfy (const True)
  case c of
    '"'  => pure '"'
    '\\' => pure '\\'
    '/'  => pure '/'
    'b'  => pure '\b'
    'f'  => pure '\f'
    'n'  => pure '\n'
    'r'  => pure '\r'
    't'  => pure '\t'
    'u'  => map chr hexQuad
    _    => satisfy (const False) <?> "expected special char"


-- ------------------------------------------------------ [ Scientific Numbers ]
-- Borrowed from Lightyear JSON Examples
-- inspired by Haskell's Data.Scientific module

record Scientific : Type where
  MkScientific : (coefficient : Integer)
                 -> (exponent : Integer)
                 -> Scientific

scientificToFloat : Scientific -> Float
scientificToFloat (MkScientific c e) = fromInteger c * exp
  where
    exp = if e < 0
            then 1 / pow 10 (fromIntegerNat (- e))
            else pow 10 (fromIntegerNat e)

parseScientific : Parser Scientific
parseScientific = do
    sign <- maybe 1 (const (-1)) `map` opt (char '-')
    digits <- some digit
    hasComma <- isJust `map` opt (char '.')
    decimals <- if hasComma
                  then some digit
                  else pure Prelude.List.Nil
    hasExponent <- isJust `map` opt (char 'e')
    exponent <- if hasExponent
                 then integer
                 else pure 0
    pure $ MkScientific (sign * fromDigits (digits ++ decimals))
                        (exponent - cast (length decimals))
  where
    fromDigits : List (Fin 10) -> Integer
    fromDigits = foldl (\a, b => 10 * a + cast b) 0
-- --------------------------------------------------------------------- [ EOF ]
