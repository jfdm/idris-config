-- --------------------------------------------------------------- [ Utils.idr ]
-- Description : Utility parsing functions.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Parse.Utils

import Lightyear
import Lightyear.Char
import Lightyear.Strings

%access export

-- ------------------------------------------------------------------- [ Stuff ]
-- These should be merged into Lightyear

word : Parser String
word = map pack (some $ alphaNum) <?> "Word"

ascii : Parser Char
ascii = do
  c <- satisfy (const True)
  case c of
    ','       => satisfy (const False)
    '{'       => satisfy (const False)
    '}'       => satisfy (const False)
    '['       => satisfy (const False)
    ']'       => satisfy (const False)
    otherwise => if ord c >= 33 && ord c <= 176
              then pure c
              else satisfy (const False)

asciiSeq : Parser String
asciiSeq = map pack (some ascii) <?> "Ascii String sans space and braces"

comment : String -> Parser ()
comment str = do
    string str
    manyTill anyChar endOfLine
    pure ()
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

public export
record Scientific where
  constructor MkScientific
  coefficient : Integer
  exponent : Integer

scientificToFloat : Scientific -> Double
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
