-- ----------------------------------------------------------------- [ INI.idr ]
-- Description : Read INI files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.INI

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Config.Parse.Common
import Config.Parse.Utils
import Config.Parse.Reader

%access private

-- ------------------------------------------------------------------- [ Model ]

public
data INIElem = INIFile (List INIElem)
             | INIComment String
             | INIEntry String String
             | INISection String (List INIElem)

-- ------------------------------------------------------------------ [ Parser ]
comment : Parser INIElem
comment = do
      xs <- body
      pure $ INIComment xs
    <?> "INI Comments"
  where
    body = commentLine ";" <|> commentLine "#"

kvpair : Parser INIElem
kvpair = do
    (k,v) <- keyvalue "=" (map pack $ manyTill anyChar eol)
    pure $ INIEntry k v
  <?> "INI KVPair"

section : Parser INIElem
section = do
      name <- brackets word
      is <- some body
      pure $ INISection name is
    <?> "Section"
  where
    body = comment <|> kvpair <?> "INI Section Body"

iniElem : Parser INIElem
iniElem = comment <|> kvpair <|> section <?> "INI Elememnt"

public
parseINI : Parser INIElem
parseINI = do
    es <- some (space $> iniElem) <$ space
    pure $ INIFile es
  <?> "INI File"

-- -------------------------------------------------------------------- [ Read ]
public
readINIConfig : String -> {[FILE_IO ()]} Eff (Either String INIElem)
readINIConfig = readConfigFile parseINI

-- --------------------------------------------------------------------- [ EOF ]
