-- ----------------------------------------------------------------- [ INI.idr ]
-- Description : Read INI files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.INI

import Lightyear
import Lightyear.Strings

import public Config.Effs
import public Config.Error

import Config.Parse.Utils
import Config.Parse.Common
import Config.Parse.Reader

%access public

-- ------------------------------------------------------------------- [ Model ]

data INIElem = INIFile (List INIElem)
             | INIEntry String String
             | INISection String (List INIElem)

instance Show INIElem where
  show (INIFile is)   = show is
  show (INIEntry k v) = show k ++ " = " ++ show v ++ "\n"
  show (INISection t kvs) = "[" ++ t ++ "]\n" ++ show kvs

-- ------------------------------------------------------------------ [ Parser ]

private
iniComment : Monad m => ParserT m String ()
iniComment = comment ";" <|> comment "#"

private
iniSpace : Monad m => ParserT m String ()
iniSpace = langSpace iniComment <?> "INI Space"

private
kvpair : Parser INIElem
kvpair = do
    (k,v) <- keyvalue "=" (map pack $ manyTill (anyChar) (eol <|> iniComment))
    pure $ INIEntry k v
  <?> "INI KVPair"

private
section : Parser INIElem
section = do
      name <- brackets word
      is <- some body
      pure $ INISection name is
    <?> "Section"
  where
    body : Parser INIElem
    body = iniSpace *> kvpair <?> "INI Section Body"

private
iniElem : Parser INIElem
iniElem = kvpair <|> section <?> "INI Elememnt"


parseINI : Parser INIElem
parseINI = do
    es <- some (iniSpace *> iniElem)
    pure $ INIFile es
  <?> "INI File"

-- -------------------------------------------------------------------- [ Read ]

readINIConfig : String -> Eff (Either ConfigError INIElem) ConfigEffs
readINIConfig = readConfigFile parseINI

-- --------------------------------------------------------------------- [ EOF ]
