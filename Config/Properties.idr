-- ---------------------------------------------------------- [ Properties.idr ]
-- Description : Properties files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Properties

import Control.Monad.Identity

import Data.SortedMap

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Config.Parse.Common
import Config.Parse.Utils
import Config.Parse.Reader

%access private

-- ------------------------------------------------------------------- [ Model ]
public
data Property = PropFile (List Property)
              | PropComment String
              | PropEntry String String

-- ------------------------------------------------------------------ [ Parser ]
comment : Parser Property
comment = do
      xs <- body
      pure $ PropComment xs
    <?> "Property Comments"
  where
    body = commentLine "#" <|> commentLine "!"

genKVpair : String -> Parser Property
genKVpair s = do
    (k,v) <- keyvalue s (map pack $ manyTill anyChar eol)
    pure $ PropEntry k v
  <?> "Proeprty KVPair"

kvpair : Parser Property
kvpair = genKVpair "=" <|> genKVpair ":"

propElem : Parser Property
propElem = kvpair <|> comment <?> "Property Element"

public
parseProperties : Parser Property
parseProperties = do
    es <- some (space $> propElem) <$ space
    pure $ PropFile es
  <?> "Properties File"

-- -------------------------------------------------------------------- [ Read ]
public
readPropertiesConfig : String -> {[FILE_IO ()]} Eff (Either String Property)
readPropertiesConfig = readConfigFile parseProperties

-- --------------------------------------------------------------------- [ EOF ]
