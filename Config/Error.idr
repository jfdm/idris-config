-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Config.Error

%access public

data ConfigError : Type where
  ParseError   : String -> ConfigError
  FileNotFound : String -> ConfigError

instance Show ConfigError where
  show (ParseError err) = unlines ["Parse Error", err]
  show (FileNotFound fname) = unwords ["File", show fname, "Not Found"]


-- --------------------------------------------------------------------- [ EOF ]
