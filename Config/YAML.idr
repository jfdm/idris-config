-- ---------------------------------------------------------------- [ YAML.idr ]
-- Description : Parse YAML files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.YAML

import Effects
import Effect.File
import Effect.Exception

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import public Config.Error

import public Config.Parse.Common
import public Config.Parse.Utils

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
public export
data YAMLNode : Type where
  -- Value Types
  YAMLNull   : YAMLNode
  YAMLString : String -> YAMLNode
  YAMLInt    : Int    -> YAMLNode
  YAMLFloat  : Double -> YAMLNode
  YAMLBool   : Bool   -> YAMLNode
  -- Node Types
  YAMLScalar : String -> YAMLNode
  YAMLSeq    : List YAMLNode -> YAMLNode
  YAMLMap    : List (YAMLNode, YAMLNode) -> YAMLNode
  -- Documents
  YAMLDoc    : List (String, String) -> YAMLNode -> YAMLNode

normaliseLiterals : String -> String
normaliseLiterals s = s

public export
Show YAMLNode where
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

showUnTyped : YAMLNode -> String
showUnTyped YAMLNull       = "null"
showUnTyped (YAMLString x) = x
showUnTyped (YAMLInt i)    = show i
showUnTyped (YAMLFloat f)  = show f
showUnTyped (YAMLBool b)   = show b
-- Node Types
showUnTyped (YAMLScalar s) = show (normaliseLiterals s)
showUnTyped (YAMLSeq ys)   = unwords ["["
      , unwords $ intersperse "," (map showUnTyped ys)
      , "]"]
showUnTyped (YAMLMap ys)   = unwords ["{"
      , unwords (intersperse "," (map showUnTypedKV ys))
      ,"}"]
   where
     showUnTypedKV : (YAMLNode, YAMLNode) -> String
     showUnTypedKV (k,v) = with List concat [showUnTyped k, ": ", showUnTyped  v]
-- Documents
showUnTyped (YAMLDoc _ x) = unlines ["%YAML 1.2","---", showUnTyped x,"..."]

public export
implementation [yamlUnTyped] Show YAMLNode where
  show x = showUnTyped x

-- ------------------------------------------------------------------ [ Parser ]

-- [ Values ]
yamlString : Parser YAMLNode
yamlString = map YAMLString word <?> "YAML String"

yamlNull : Parser YAMLNode
yamlNull = token "null" >! pure YAMLNull <?> "YAML Null"

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
        ws <- quoted c
        spaces
        pure $ YAMLScalar $ ws
      <?> "YAML Scalar General"

yamlFlowValue : Parser YAMLNode
yamlFlowValue = yamlNull
            <|> yamlBool
            <|> yamlNum
            <|> yamlQuotedScalar
            <|> yamlStrs
             <?> "YAML Primitives"
  where
    yamlStrs : Parser YAMLNode
    yamlStrs = do
      ws <- some $ lexeme asciiSeq
      pure $ YAMLString $ unwords ws

-- [ Nodes ]
yamlFlowSeq : Parser YAMLNode
yamlFlowSeq = do
    xs <- brackets (commaSep (lexeme yamlFlowValue))
    spaces
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
    spaces
    pure $ YAMLMap xs
  <?> "YAML Flow Map"


yamlSentance : Parser YAMLNode
yamlSentance = do
  ws <- manyTill (spaces *> word) endOfLine
  pure $ YAMLString $ unwords ws

-- ------------------------------------------------------------------ [ Blocks ]
yamlObject : Parser YAMLNode
yamlObject = yamlNull <|> yamlBool <|> yamlNum <|> yamlQuotedScalar
         <|> yamlSentance <|> yamlFlowSeq <|> yamlFlowMap
         <?> "YAMLValue"

yamlBlockSeq : Parser YAMLNode
yamlBlockSeq = do
    xs <- some (token "-" *!> yamlObject <* spaces)
    pure $ YAMLSeq xs
  <?> "YAML List Sequence"

yamlBlockKVPair : Parser (YAMLNode, YAMLNode)
yamlBlockKVPair = do
    key <- yamlString
    colon
    spaces
    value <- yamlObject
    pure $ (key, value)
  <?> "YAML Block KV Pair"

yamlBlockMap : Parser YAMLNode
yamlBlockMap = do
    xs <- some (yamlBlockKVPair <* spaces)
    pure $ YAMLMap xs
  <?> "Map Block"

yamlDirective : Parser (String, String)
yamlDirective = do
    string "%"
    k <- word
    spaces
    v <- manyTill anyChar endOfLine
    pure $ (k, pack v)
  <?> "YAML DIrective"


||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Block Map and Block Seq, these only take flow nodes.
export
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
export
parseYAMLStream : Parser (List YAMLNode)
parseYAMLStream = some parseYAMLDoc

-- ------------------------------------------------------------------ [ String ]
export
toString : YAMLNode -> String
toString doc = show @{yamlUnTyped} doc

export
toStringTyped : YAMLNode -> String
toStringTyped doc = show doc

||| Requires that the file is untyped
|||
||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
export
fromString : String -> Either ConfigError YAMLNode
fromString str =
    case parse parseYAMLDoc str of
      Left err  => Left (PureParseErr err)
      Right doc => Right doc

-- -------------------------------------------------------------------- [ Read ]

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
export
readYAMLConfig : String -> Eff (Either ConfigError YAMLNode) [FILE ()]
readYAMLConfig = readConfigFile parseYAMLDoc

||| This does not recognise:
|||  + keep or strip options
|||  + indent options
|||  + Inline Comments.
|||  + Scalar Blocks
|||  + Complext Map and Seq Blocks
export
readYAMLStream : String -> Eff (List YAMLNode) [FILE ()]
readYAMLStream inStr =
    case !(readConfigFile parseYAMLStream inStr) of
      Left _   => pure Nil
      Right ds => pure ds

-- --------------------------------------------------------------------- [ EOF ]
