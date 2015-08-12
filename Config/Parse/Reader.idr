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

readFile : { ['configfile ::: FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { ['configfile ::: FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !('configfile :- eof))
                     then readAcc (acc ++ !('configfile :- readLine))
                     else pure acc

readConfigFile : Parser a
              -> String
              -> Eff a ConfigEffs
readConfigFile p f = do
    case !('configfile :- open f Read) of
      True => do
        src <- readFile
        'configfile :- close
        case parse p src of
          Left err  => Config.raise (ParseError err)
          Right res => pure res
      False => Config.raise (FileNotFound f)
-- --------------------------------------------------------------------- [ EOF ]
