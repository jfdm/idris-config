-- ---------------------------------------------------------------- [ YAML.idr ]
-- Description : Parse YAML files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.YAML

import public Lightyear
import public Lightyear.Strings

import public Data.SortedMap

import public Config.Effs

import Config.Parse.Common
import Config.Parse.Utils
import Config.Parse.Reader

import public Config.Error

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

private
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

instance [yamlUnTyped] Show YAMLNode where
  -- Value Types
  show YAMLNull       = "\"null\""
  show (YAMLString x) = show x
  show (YAMLInt i)    = show i
  show (YAMLFloat f)  = show f
  show (YAMLBool b)   = show b
  -- Node Types
  show (YAMLScalar s) = show (normaliseLiterals s)
  show (YAMLSeq ys)   = show ys
  show (YAMLMap ys)   = "{" ++
      unwords (intersperse "," (map showKV ys))++ "}"
     where
       showKV : (YAMLNode, YAMLNode) -> String
       showKV (k,v) = show k ++ " : " ++ show v
  -- Documents
  show (YAMLDoc _ x) = "%YAML 1.2\n---\n" ++ show x ++ "\n...\n"

-- ------------------------------------------------------------------ [ Parser ]

-- [ Values ]
private
yamlString : Parser YAMLNode
yamlString = map YAMLString word <?> "YAML String"

private
yamlNull : Parser YAMLNode
yamlNull = token "null" >! return YAMLNull <?> "YAML Null"

private
yamlBool : Parser YAMLNode
yamlBool = do token "false"; pure $ YAMLBool False
       <|> do token "true";  pure $ YAMLBool True
       <?> "YAML Boolean"

private
yamlFloat : Parser YAMLNode
yamlFloat = map YAMLFloat $ map scientificToFloat parseScientific <?> "YAML Floats"

private
yamlInt : Parser YAMLNode
yamlInt = do
    num <- map pack (some $ satisfy isDigit)
    pure $ YAMLInt (cast num)
  <?> "YAML Int"

private
yamlNum : Parser YAMLNode
yamlNum = yamlFloat <|> yamlInt <?> "YAML Num"

-- [ Scalars ]

private
yamlQuotedScalar : Parser YAMLNode
yamlQuotedScalar = yamlQuoteGen '\'' <|> yamlQuoteGen '\"' <?> "YAML Qoted Scalar"
  where
    yamlQuoteGen : Char -> Parser YAMLNode
    yamlQuoteGen c = do
        ws <- literallyBetween c
        space
        pure $ YAMLScalar $ ws
      <?> "YAML Scalar General"

private
yamlFlowValue : Parser YAMLNode
yamlFlowValue = yamlNull <|> yamlBool <|> yamlNum
            <|> yamlQuotedScalar <|> yamlStrs
             <?> "YAML Primitives"
  where
    yamlStrs : Parser YAMLNode
    yamlStrs = do
      ws <- some $ lexeme asciiSeq
      pure $ YAMLString $ unwords ws

-- [ Nodes ]
private
yamlFlowSeq : Parser YAMLNode
yamlFlowSeq = do
    xs <- brackets (commaSep (lexeme yamlFlowValue))
    space
    pure $ YAMLSeq xs
   <?> "YAML Flow Sequence"

private
yamlKVPair : Parser (YAMLNode, YAMLNode)
yamlKVPair = do
   key <- yamlString
   colon
   value <- yamlFlowValue
   pure $ (key, value)
  <?> "YAML KV Pair Flow"

private
yamlFlowMap : Parser YAMLNode
yamlFlowMap = do
    xs <- braces (commaSep (lexeme yamlKVPair))
    space
    pure $ YAMLMap xs
  <?> "YAML Flow Map"


private
yamlSentance : Parser YAMLNode
yamlSentance = do
  ws <- manyTill (space *> word) eol
  pure $ YAMLString $ unwords ws

-- ------------------------------------------------------------------ [ Blocks ]
private
yamlObject : Parser YAMLNode
yamlObject = yamlNull <|> yamlBool <|> yamlNum <|> yamlQuotedScalar
         <|> yamlSentance <|> yamlFlowSeq <|> yamlFlowMap
         <?> "YAMLValue"

private
yamlBlockSeq : Parser YAMLNode
yamlBlockSeq = do
    xs <- some (token "-" *!> yamlObject <* space)
    pure $ YAMLSeq xs
  <?> "YAML List Sequence"

private
yamlBlockKVPair : Parser (YAMLNode, YAMLNode)
yamlBlockKVPair = do
    key <- yamlString
    colon
    space
    value <- yamlObject
    pure $ (key, value)
  <?> "YAML Block KV Pair"

private
yamlBlockMap : Parser YAMLNode
yamlBlockMap = do
    xs <- some (yamlBlockKVPair <* space)
    pure $ YAMLMap xs
  <?> "Map Block"

private
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
parseYAMLDoc : Parser YAMLNode
parseYAMLDoc = do
    ds <- some yamlDirective
    token "---"
    b <- body
    opt $ token "..."
    pure $ YAMLDoc ds b
   <?> "YAML Document"
  where
    body : Parser YAMLNode
    body = yamlBlockMap <|> yamlBlockSeq

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
parseYAMLStream : Parser (List YAMLNode)
parseYAMLStream = some parseYAMLDoc

-- ------------------------------------------------------------------ [ String ]

toString : YAMLNode -> String
toString doc = show doc

toStringTyped : YAMLNode -> String
toStringTyped doc = show @{yamlUnTyped} doc

||| Requires that the file is untyped
|||
||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
fromString : String -> Either String YAMLNode
fromString str = parse parseYAMLDoc str

-- -------------------------------------------------------------------- [ Read ]

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
readYAMLConfig : String -> Eff (YAMLNode) ConfigEffs
readYAMLConfig = readConfigFile parseYAMLDoc

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
readYAMLStream : String -> Eff (List YAMLNode) ConfigEffs
readYAMLStream = readConfigFile parseYAMLStream

-- --------------------------------------------------------------------- [ EOF ]
