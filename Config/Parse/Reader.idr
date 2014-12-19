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

import public Effects
import public Effect.File
import public Effect.StdIO

%access private

readFile : { [FILE_IO (OpenFile Read)] } Eff String
readFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readConfigFile : Parser a -> String -> { [FILE_IO ()] } Eff (Either String a)
readConfigFile p f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => pure $ Left err
          Right res => pure $ Right res
      False => pure $ Left "Error"
-- --------------------------------------------------------------------- [ EOF ]
