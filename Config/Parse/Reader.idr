-- -------------------------------------------------------------- [ Reader.idr ]
-- Description : Common functions for reading in config files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Parse.Reader

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Effects
import Effect.File
import Effect.StdIO
import Effect.Exception

%access private

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readConfigFile : Parser a -> String -> { [EXCEPTION String, FILE_IO ()] } Eff a
readConfigFile p f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => raise $ show err
          Right res => pure res
      False => raise "Error"
-- --------------------------------------------------------------------- [ EOF ]
