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
  FileNotFound : String -> ConfigError

Show ConfigError where
  show (PureParseErr err)   = unlines ["Parse Error:", err]
  show (ParseError fn err)  = unlines ["Parse Error:", fn, err]
  show (FileNotFound fname) = unwords ["File", show fname, "Not Found"]


-- --------------------------------------------------------------------- [ EOF ]
