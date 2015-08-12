-- -------------------------------------------------------------- [ Reader.idr ]
-- Description : Common functions for reading in config files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Parse.Reader

import Lightyear
import Lightyear.Strings

import Config.Effs

%access public

private
readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readConfigFile : Parser a
              -> String
              -> Eff a ConfigEffs
readConfigFile p f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => Config.raise (ParseError err)
          Right res => pure res
      False => Config.raise (FileNotFound f)
-- --------------------------------------------------------------------- [ EOF ]
