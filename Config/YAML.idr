-- ---------------------------------------------------------------- [ YAML.idr ]
-- Description : Parse YAML files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.YAML

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Data.HVect
import Data.SortedMap

import Config.Parse.Common
import Config.Parse.Utils
import Config.Parse.Reader

%access public

-- ------------------------------------------------------------------- [ Model ]

||| YAML Documentation representation.
|||
||| There is no support for:
||| + Anchors
||| + Complex Maps
||| + Schema's
||| + User Defined tags
||| + Interesting Scalar Formats
||| + Scalar blocks must end with two EOL.
||| + Directives aside from DTD
public
data YAMLNode : Type where
  -- Value Types
  YAMLNull   : YAMLNode
  YAMLString : String -> YAMLNode
  YAMLInt    : Int -> YAMLNode
  YAMLFloat  : Float -> YAMLNode
  YAMLBool   : Bool -> YAMLNode
  -- Node Types
  YAMLScalar : String -> YAMLNode
  YAMLSeq    : List YAMLNode -> YAMLNode
  YAMLMap    : (SortedMap String YAMLNode) -> YAMLNode
  -- Documents
  YAMLDoc    : List (String, String) -> YAMLNode -> YAMLNode

normaliseLiterals : String -> String
normaliseLiterals s = s

instance Show YAMLNode where
  -- Value Types
  show YAMLNull       = "!!null \"null\""
  show (YAMLString x) = "!!str " ++ show x
  show (YAMLInt i)    = "!!int " ++ show i
  show (YAMLFloat f)  = "!!float " ++ show f
  show (YAMLBool b)   = "!!bool " ++ show b
  -- Node Types
  show (YAMLScalar s) = "!!str " ++ show (normaliseLiterals s)
  show (YAMLSeq ys)   = "!!seq " ++ show ys
  show (YAMLMap ys)   = "!!map " ++ "{" ++
      unwords (intersperse "," (map showKV $ SortedMap.toList ys))++ "}"
     where
       showKV : (String, YAMLNode) -> String
       showKV (k,v) = show k ++ " : " ++ show v
  -- Documents
  show (YAMLDoc _ x) = "%YAML 1.2\n---\n" ++ show x ++ "\n...\n"

-- ------------------------------------------------------------------ [ Parser ]

-- [ Values ]
yamlString : Parser YAMLNode
yamlString = do
    ws <- some (lexeme word)
    pure $ YAMLString $ unwords ws
  <?> "YAML String"

yamlNull : Parser YAMLNode
yamlNull = token "null" >! return YAMLNull <?> "YAML Null"

yamlBool : Parser YAMLNode
yamlBool = do token "false"; pure $ YAMLBool False
       <|> do token "true";  pure $ YAMLBool True
       <?> "YAML Boolean"

yamlFloat : Parser YAMLNode
yamlFloat = map YAMLFloat $ map scientificToFloat parseScientific <?> "YAML Floats"

yamlInt : Parser YAMLNode
yamlInt = do
    num <- map pack (some $ satisfy isDigit)
    pure $ YAMLInt (cast num)
  <?> "YAML Int"

yamlNum : Parser YAMLNode
yamlNum = yamlInt <|> yamlFloat <?> "YAML Num"

-- [ Scalars ]

yamlBlockScalar : Parser YAMLNode
yamlBlockScalar = yamlScalarBlockGen '>'
              <|> yamlScalarBlockGen '|'
              <?> "YAML Scalar Block"
  where
    yamlScalarBlockGen : Char -> Parser YAMLNode
    yamlScalarBlockGen c = do
        char c
        ws <- map pack $ manyTill anyChar (eol $> eol)
        pure $ YAMLScalar ws
      <?> "YAML Block General"

yamlQuotedScalar : Parser YAMLNode
yamlQuotedScalar = yamlQuoteGen '\'' <|> yamlQuoteGen '\"' <?> "YAML Qoted Scalar"
  where
    yamlQuoteGen : Char -> Parser YAMLNode
    yamlQuoteGen c = do
        ws <- literallyBetween c
        pure $ YAMLScalar $ ws
      <?> "YAML Scalar General"

yamlScalar : Parser YAMLNode
yamlScalar = yamlQuotedScalar <|> yamlBlockScalar <?> "YAML Scalar"

{-
 show (YAMLSeq ys)   = "!!seq " ++ show ys
  show (YAMLMap ys)   = "!!map " ++ "{" ++
  -}
-- [ Nodes ]
mutual
  yamlSeqInline : Parser $ List YAMLNode
  yamlSeqInline = brackets (commaSep yamlValue) <?> "YAML Inline Sequence"

  yamlKVPair : Parser $ (String, YAMLNode)
  yamlKVPair = do
    key <- yamlString
    colon
    value <- yamlValue
    pure (key, value)

  yamlMap : Parser YAMLNode
  yamlMap =

{-
% Many Directives
--- # Begin Document


... # End DOcument
-}


{-
Alias nodes are: &<label> => def *<label>
Empty Nodes are empty
Scalars are: SQuote, DQuote, Raw
Collections:
  Seq are denoted by []
  Maps are denoted by {(key :value)+}
Blocks are Either Literal (|) or Folded (>)
 With Indicator Indent 1
 With Indicator Chomp
   Keep  +
   Clip Default
   Strip -
-}
{-

||| This does not recognise:
|||  + keep or strip options
|||  + indent options

-- ||| + Inline Comments.
-}

-- --------------------------------------------------------------------- [ EOF ]
