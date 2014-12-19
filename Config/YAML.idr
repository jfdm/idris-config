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
yamlNum = yamlFloat <|> yamlInt <?> "YAML Num"

-- [ Scalars ]

yamlQuotedScalar : Parser YAMLNode
yamlQuotedScalar = yamlQuoteGen '\'' <|> yamlQuoteGen '\"' <?> "YAML Qoted Scalar"
  where
    yamlQuoteGen : Char -> Parser YAMLNode
    yamlQuoteGen c = do
        ws <- literallyBetween c
        pure $ YAMLScalar $ ws
      <?> "YAML Scalar General"

-- [ Nodes ]
mutual
  yamlSeqInline : Parser YAMLNode
  yamlSeqInline = do
    xs <- brackets (commaSep (lexeme yamlValue)) <?> "YAML Inline Sequence"
    pure $ YAMLSeq xs

  yamlKVPair : Parser (String, YAMLNode)
  yamlKVPair = do
     key <- (some $ lexeme word)
     colon
     value <- yamlValue
     pure $ (unwords key, value)
    <?> "YAML KV Pair Flow"

  yamlMap : Parser YAMLNode
  yamlMap = do
      xs <- braces (commaSep (yamlKVPair <$ space)) <?> "YAML Map"
      pure $ YAMLMap $ fromList xs
    <?> "YAML Map FLow"

  yamlValue : Parser YAMLNode
  yamlValue = yamlString <|> yamlNull <|> yamlBool <|> yamlNum <|> yamlSeqInline <|> yamlMap <?> "YAMLValue"

yamlSeqList : Parser YAMLNode
yamlSeqList = do
  xs <- some (token "-" $!> yamlValue <$ space ) <?> "YAML List Sequence"
  pure $ YAMLSeq xs

yamlBlockKVPair : Parser (String, YAMLNode)
yamlBlockKVPair = do
    key <- manyTill word colon
    eol
    value <- yamlValue
    pure $ ((unwords key), value)
  <?> "YAML Block KV Pair"

yamlMapBlock : Parser YAMLNode
yamlMapBlock = do
    xs <- some (yamlBlockKVPair <$ space)
    pure $ YAMLMap $ fromList xs
  <?> "Map Block"

yamlDirective : Parser (String, String)
yamlDirective = do
    string "%"
    k <- word
    space
    v <- manyTill anyChar eol
    pure $ (k, pack v)
  <?> "YAML DIrective"

public
parseYAMLDoc : Parser YAMLNode
parseYAMLDoc = do
    ds <- some yamlDirective
    token "---"
    b <- body
    opt $ token "..."
    pure $ YAMLDoc ds b
   <?> "YAML Document"
  where
    body = yamlMapBlock <|> yamlSeqList

public
parseYAMLStream : Parser (List YAMLNode)
parseYAMLStream = some parseYAMLDoc

-- -------------------------------------------------------------------- [ Read ]
public
readYAMLConfig : String -> {[FILE_IO ()]} Eff (Either String YAMLNode)
readYAMLConfig = readConfigFile parseYAMLDoc

public
readYAMLStream : String -> {[FILE_IO ()]} Eff (Either String (List YAMLNode))
readYAMLStream = readConfigFile parseYAMLStream

-- --------------------------------------------------------------------- [ EOF ]


{-
% Many Directives
--- # Begin Document


... # End DOcument
-}

{-

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar BLocks
||| + Scalar blocks must end with two EOL.
-}
-- --------------------------------------------------------------------- [ EOF ]
