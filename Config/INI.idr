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
             | INIEntry String String
             | INISection String (List INIElem)

instance Show INIElem where
  show (INIFile is)   = show is
  show (INIEntry k v) = show k ++ " = " ++ show v ++ "\n"
  show (INISection t kvs) = "[" ++ t ++ "]\n" ++ show kvs

-- ------------------------------------------------------------------ [ Parser ]
iniComment : Monad m => ParserT m String ()
iniComment = comment ";" <|> comment "#" <?> "INI Comments"

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
    body = kvpair <?> "INI Section Body"

iniElem : Parser INIElem
iniElem = kvpair <|> section <?> "INI Elememnt"

public
parseINI : Parser INIElem
parseINI = do
    es <- some (iniComment $> space $> iniElem) <$ space
    pure $ INIFile es
  <?> "INI File"

-- -------------------------------------------------------------------- [ Read ]
public
readINIConfig : String -> {[FILE_IO ()]} Eff (Either String INIElem)
readINIConfig = readConfigFile parseINI

-- --------------------------------------------------------------------- [ EOF ]
