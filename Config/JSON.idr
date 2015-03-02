-- ---------------------------------------------------------------- [ JSON.idr ]
-- Description : Parse JSON files.
--               This code was borrowed and improved from lightyear examples.
--
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.JSON

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
public
data JsonValue = JsonString String
               | JsonNumber Float
               | JsonBool Bool
               | JsonNull
               | JsonArray (List JsonValue)
               | JsonObject (SortedMap String JsonValue)

instance Show JsonValue where
  show (JsonString s)   = show s
  show (JsonNumber x)   = show x
  show (JsonBool True ) = "true"
  show (JsonBool False) = "false"
  show  JsonNull        = "null"
  show (JsonArray  xs)  = show xs
  show (JsonObject xs)  =
      "{" ++ unwords (intersperse "," (map fmtItem $ SortedMap.toList xs)) ++ "}"
    where
      fmtItem (k, v) = show k ++ " : " ++ show v

-- ------------------------------------------------------------------ [ Parser ]
jsonString : Parser String
jsonString = literallyBetween '"' <?> "JSON String"

jsonNumber : Parser Float
jsonNumber = map scientificToFloat parseScientific <?> "JSON Number"

jsonBool : Parser Bool
jsonBool  =  (char 't' >! string "rue"  *> return True)
         <|> (char 'f' >! string "alse" *> return False)
         <?> "JSON Bool"

jsonNull : Parser ()
jsonNull = (char 'n' >! string "ull" >! return ()) <?> "JSON Null"

mutual
  jsonArray : Parser (List JsonValue)
  jsonArray = brackets (commaSep jsonValue) <?> "JSON Array"

  keyValuePair : Parser (String, JsonValue)
  keyValuePair = do
      key <- space *> jsonString <* space
      colon
      value <- jsonValue
      pure (key, value)
    <?> "JSON KV Pair"

  jsonObject : Parser (SortedMap String JsonValue)
  jsonObject = map fromList $ braces (commaSep (keyValuePair)) <?> "JSON Object"

  jsonValue' : Parser JsonValue
  jsonValue' =  (map JsonString jsonString)
            <|> (map JsonNumber jsonNumber)
            <|> (map JsonBool   jsonBool)
            <|> (pure JsonNull <* jsonNull)
            <|>| map JsonArray  jsonArray
            <|>| map JsonObject jsonObject

  jsonValue : Parser JsonValue
  jsonValue = space *> jsonValue' <* space <?> "JSON Value"

public
parseJSONFile : Parser JsonValue
parseJSONFile = (map JsonArray jsonArray)
            <|> (map JsonObject jsonObject)
            <?> "JSON Files"

-- -------------------------------------------------------------------- [ Read ]
public
readJSONConfig : String -> {[EXCEPTION String, FILE_IO ()]} Eff JsonValue
readJSONConfig = readConfigFile parseJSONFile

-- --------------------------------------------------------------------- [ EOF ]
