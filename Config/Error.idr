-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Error

%access public export

data ConfigError : Type where
  PureParseErr : String -> ConfigError
  ParseError   : String -> String -> ConfigError
  FileNotFound : String -> FileError -> ConfigError

Show ConfigError where
  show (PureParseErr err)   = unlines ["Parse Error:", err]
  show (ParseError fn err)  =
    unlines [ unwords ["Parse Error:", fn, "error was:"]
            , err
            ]
  show (FileNotFound fname err) =
    unlines [ unwords ["File", show fname, "caused error:"]
            , show err
            ]


-- --------------------------------------------------------------------- [ EOF ]
