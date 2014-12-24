-- ----------------------------------------------------------------- [ INI.idr ]
-- Description : Read INI files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.INI

import public Effects
import public Effect.File
import public Effect.Stdio
import public Effect.Exception

import public Control.Monad.Identity

import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import Config.Parse.Utils
import Config.Parse.Common
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
iniComment = comment ";" <|> comment "#"

iniSpace : Monad m => ParserT m String ()
iniSpace = langSpace iniComment <?> "INI Space"

kvpair : Parser INIElem
kvpair = do
    (k,v) <- keyvalue "=" (map pack $ manyTill (anyChar) (eol <|> iniComment))
    pure $ INIEntry k v
  <?> "INI KVPair"

section : Parser INIElem
section = do
      name <- brackets word
      is <- some body
      pure $ INISection name is
    <?> "Section"
  where
    body = iniSpace $> kvpair <?> "INI Section Body"

iniElem : Parser INIElem
iniElem = kvpair <|> section <?> "INI Elememnt"

public
parseINI : Parser INIElem
parseINI = do
    es <- some (iniSpace $> iniElem)
    pure $ INIFile es
  <?> "INI File"

-- -------------------------------------------------------------------- [ Read ]
public
readINIConfig : String -> {[FILE_IO ()]} Eff (Either String INIElem)
readINIConfig = readConfigFile parseINI

-- --------------------------------------------------------------------- [ EOF ]
