-- -------------------------------------------------------------- [ Common.idr ]
-- Description : Common Parsing Functions
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Parse.Common

import Effects
import Effect.File

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.StringFile

import Config.Parse.Utils
import Config.Error

%access export

langSpace : Parser () -> Parser ()
langSpace p = p <|> spaces <?> "Space Lang"

keyvalue : String
         -> Parser String
         -> Parser (String, String)
keyvalue s value = do
    k <- word
    spaces
    token s
    v <- value
    spaces
    pure (k,v)
  <?> "KVPair"

readConfigFile : Parser a
              -> String
              -> Eff (Either ConfigError a) [FILE ()]
readConfigFile p f = parseFile FileNotFound ParseError p f

-- --------------------------------------------------------------------- [ EOF ]
