-- ---------------------------------------------------------- [ Properties.idr ]
-- Description : Properties files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Properties

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
data Property = PropFile (List Property)
              | PropEntry String String

-- ------------------------------------------------------------------ [ Parser ]
propComment : Parser ()
propComment = comment "#" <|> comment "!" <?> "Property Comments"

propSpace : Parser ()
propSpace = langSpace propComment <?> "Prop Space"

genKVpair : String -> Parser Property
genKVpair s = do
    (k,v) <- keyvalue s (map pack $ manyTill anyChar (skip endOfLine <|> propComment))
    pure $ PropEntry k v
  <?> "Proeprty KVPair"

kvpair : Parser Property
kvpair = genKVpair "=" <|> genKVpair ":"

export
parseProperties : Parser Property
parseProperties = do
    es <- some (propSpace *> kvpair)
    pure $ PropFile es
  <?> "Properties File"

-- -------------------------------------------------------------------- [ Read ]
export
readPropertiesConfig : String -> Eff (Either ConfigError Property) [FILE ()]
readPropertiesConfig = readConfigFile parseProperties

-- --------------------------------------------------------------------- [ EOF ]
