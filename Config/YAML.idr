-- ---------------------------------------------------------------- [ YAML.idr ]
-- Description : Parse YAML files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.YAML

import public Effects
import public Effect.File
import public Effect.StdIO
import public Effect.Exception

import public Control.Monad.Identity

import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import public Data.SortedMap

import Config.Parse.Common
import Config.Parse.Utils
import Config.Parse.Reader

%access private

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
  YAMLMap    : List (YAMLNode, YAMLNode) -> YAMLNode
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
      unwords (intersperse "," (map showKV ys))++ "}"
     where
       showKV : (YAMLNode, YAMLNode) -> String
       showKV (k,v) = show k ++ " : " ++ show v
  -- Documents
  show (YAMLDoc _ x) = "%YAML 1.2\n---\n" ++ show x ++ "\n...\n"

-- ------------------------------------------------------------------ [ Parser ]

-- [ Values ]
yamlString : Parser YAMLNode
yamlString = map YAMLString word <?> "YAML String"

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
        space
        pure $ YAMLScalar $ ws
      <?> "YAML Scalar General"

yamlFlowValue : Parser YAMLNode
yamlFlowValue = yamlNull <|> yamlBool <|> yamlNum
            <|> yamlQuotedScalar <|> yamlStrs
             <?> "YAML Primitives"
  where
    yamlStrs = do
      ws <- some $ lexeme word
      pure $ YAMLString $ unwords ws

-- [ Nodes ]

yamlFlowSeq : Parser YAMLNode
yamlFlowSeq = do
    xs <- brackets (commaSep (lexeme yamlFlowValue))
    space
    pure $ YAMLSeq xs
   <?> "YAML Flow Sequence"

yamlKVPair : Parser (YAMLNode, YAMLNode)
yamlKVPair = do
   key <- yamlString
   colon
   value <- yamlFlowValue
   pure $ (key, value)
  <?> "YAML KV Pair Flow"

yamlFlowMap : Parser YAMLNode
yamlFlowMap = do
    xs <- braces (commaSep (lexeme yamlKVPair))
    space
    pure $ YAMLMap xs
  <?> "YAML Flow Map"

yamlSentance : Parser YAMLNode
yamlSentance = do
  ws <- manyTill (space $> word) eol
  pure $ YAMLString $ unwords ws

-- ------------------------------------------------------------------ [ Blocks ]

yamlObject : Parser YAMLNode
yamlObject = yamlNull <|> yamlBool <|> yamlNum <|> yamlQuotedScalar
         <|> yamlSentance <|> yamlFlowSeq <|> yamlFlowMap
         <?> "YAMLValue"

yamlBlockSeq : Parser YAMLNode
yamlBlockSeq = do
    xs <- some (token "-" $!> yamlObject <$ space)
    pure $ YAMLSeq xs
  <?> "YAML List Sequence"

yamlBlockKVPair : Parser (YAMLNode, YAMLNode)
yamlBlockKVPair = do
    key <- yamlString
    colon
    space
    value <- yamlObject
    pure $ (key, value)
  <?> "YAML Block KV Pair"

yamlBlockMap : Parser YAMLNode
yamlBlockMap = do
    xs <- some (yamlBlockKVPair <$ space)
    pure $ YAMLMap xs
  <?> "Map Block"

yamlDirective : Parser (String, String)
yamlDirective = do
    string "%"
    k <- word
    space
    v <- manyTill anyChar eol
    pure $ (k, pack v)
  <?> "YAML DIrective"


||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Block Map and Block Seq, these only take flow nodes.
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
    body = yamlBlockMap <|> yamlBlockSeq

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
public
parseYAMLStream : Parser (List YAMLNode)
parseYAMLStream = some parseYAMLDoc

-- -------------------------------------------------------------------- [ Read ]

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
public
readYAMLConfig : String -> {[FILE_IO ()]} Eff (Either String YAMLNode)
readYAMLConfig = readConfigFile parseYAMLDoc

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
public
readYAMLStream : String -> {[FILE_IO ()]} Eff (Either String (List YAMLNode))
readYAMLStream = readConfigFile parseYAMLStream

-- --------------------------------------------------------------------- [ EOF ]
