-- -------------------------------------------------------------- [ Reader.idr ]
-- Description : Common functions for reading in config files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Parse.Reader

import Lightyear
import Lightyear.Strings

import Config.Effs
import Config.Error

%access public

readFile : Eff String [FILE_IO (OpenFile Read)]
readFile = readAcc ""
  where
    readAcc : String -> Eff String [FILE_IO (OpenFile Read)]
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

readConfigFile : Parser a
              -> String
              -> Eff (Either ConfigError a) ConfigEffs
readConfigFile p f =
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => pure $ Left (ParseError err)
          Right res => pure $ Right res
      False => pure $ Left (FileNotFound f)
-- --------------------------------------------------------------------- [ EOF ]
