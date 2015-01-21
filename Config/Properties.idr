-- ---------------------------------------------------------- [ Properties.idr ]
-- Description : Properties files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Properties

import public Effects
import public Effect.File
import public Effect.StdIO
import public Effect.Exception

import public Control.Monad.Identity

import public Data.SortedMap

import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import Config.Parse.Utils
import Config.Parse.Common
import Config.Parse.Reader

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
    (k,v) <- keyvalue s (map pack $ manyTill anyChar (eol <|> propComment))
    pure $ PropEntry k v
  <?> "Proeprty KVPair"

kvpair : Parser Property
kvpair = genKVpair "=" <|> genKVpair ":"

public
parseProperties : Parser Property
parseProperties = do
    es <- some (propSpace $> kvpair)
    pure $ PropFile es
  <?> "Properties File"

-- -------------------------------------------------------------------- [ Read ]
public
readPropertiesConfig : String -> {[EXCEPTION String, FILE_IO ()]} Eff Property
readPropertiesConfig = readConfigFile parseProperties

-- --------------------------------------------------------------------- [ EOF ]
