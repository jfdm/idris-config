-- ---------------------------------------------------------------- [ JSON.idr ]
-- Description : Parse JSON files.
--               This code was borrowed and improved from lightyear examples.
--
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.JSON

import public Data.AVL.Dict

import Effects
import Effect.File

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import public Config.Error

import Config.Parse.Utils
import Config.Parse.Common

%access private

-- ------------------------------------------------------------------- [ Model ]

public export
data JsonValue = JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               | JsonArray (List JsonValue)
               | JsonObject (Dict String JsonValue)

public export
Show JsonValue where
  show (JsonString s)   = show s
  show (JsonNumber x)   = show x
  show (JsonBool True ) = "true"
  show (JsonBool False) = "false"
  show  JsonNull        = "null"
  show (JsonArray  xs)  = show xs
  show (JsonObject xs)  =
      "{" ++ unwords (intersperse "," (map fmtItem $ Dict.toList xs)) ++ "}"
    where
      fmtItem (k, v) = show k ++ " : " ++ show v

-- ------------------------------------------------------------------ [ Parser ]
jsonString : Parser String
jsonString = quoted '"' <?> "JSON String"

jsonNumber : Parser Double
jsonNumber = map scientificToFloat parseScientific <?> "JSON Number"

jsonBool : Parser Bool
jsonBool  =  (char 't' >! string "rue"  *> pure True)
         <|> (char 'f' >! string "alse" *> pure False)
         <?> "JSON Bool"

jsonNull : Parser ()
jsonNull = (char 'n' >! string "ull" >! pure ()) <?> "JSON Null"

mutual
  jsonArray : Parser (List JsonValue)
  jsonArray = brackets (commaSep jsonValue) <?> "JSON Array"

  keyValuePair : Parser (String, JsonValue)
  keyValuePair = do
      key <- spaces *> jsonString <* spaces
      colon
      value <- jsonValue
      pure (key, value)
    <?> "JSON KV Pair"

  jsonObject : Parser (Dict String JsonValue)
  jsonObject = map fromList $ braces (commaSep (keyValuePair)) <?> "JSON Object"

  jsonValue' : Parser JsonValue
  jsonValue' =  (map JsonString jsonString)
            <|> (map JsonNumber jsonNumber)
            <|> (map JsonBool   jsonBool)
            <|> (pure JsonNull <* jsonNull)
            <|>| map JsonArray  jsonArray
            <|>| map JsonObject jsonObject

  jsonValue : Parser JsonValue
  jsonValue = spaces *> jsonValue' <* spaces <?> "JSON Value"

export
parseJSONFile : Parser JsonValue
parseJSONFile = (map JsonArray jsonArray)
            <|> (map JsonObject jsonObject)
            <?> "JSON Files"



export
toString : JsonValue -> String
toString doc = show doc

export
fromString : String -> Either ConfigError JsonValue
fromString str =
    case parse parseJSONFile str of
      Left err  => Left (PureParseErr err)
      Right doc => Right doc

-- -------------------------------------------------------------------- [ Read ]
export
readJSONConfig : String -> Eff (Either ConfigError JsonValue) [FILE ()]
readJSONConfig = readConfigFile parseJSONFile

-- --------------------------------------------------------------------- [ EOF ]
