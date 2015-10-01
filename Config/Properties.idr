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
public
data Property = PropFile (List Property)
              | PropEntry String String

-- ------------------------------------------------------------------ [ Parser ]
propComment : Monad m => ParserT m String ()
propComment = comment "#" <|> comment "!" <?> "Property Comments"

propSpace : Monad m => ParserT m String ()
propSpace = langSpace propComment <?> "Prop Space"

genKVpair : String -> Parser Property
genKVpair s = do
    (k,v) <- keyvalue s (map pack $ manyTill anyChar (skip endOfLine <|> propComment))
    pure $ PropEntry k v
  <?> "Proeprty KVPair"

kvpair : Parser Property
kvpair = genKVpair "=" <|> genKVpair ":"

public
parseProperties : Parser Property
parseProperties = do
    es <- some (propSpace *> kvpair)
    pure $ PropFile es
  <?> "Properties File"

-- -------------------------------------------------------------------- [ Read ]
public
readPropertiesConfig : String -> Eff (Either ConfigError Property) [FILE_IO ()]
readPropertiesConfig = readConfigFile parseProperties

-- --------------------------------------------------------------------- [ EOF ]
