-- ---------------------------------------------------------------- [ Effs.idr ]
-- Module    : Effs.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Config.Effs

import public Effects
import public Effect.File
import public Effect.StdIO
import public Effect.Exception

import public Config.Error

ConfigEffs : List EFFECT
ConfigEffs = [FILE_IO (), 'config ::: EXCEPTION ConfigError]

namespace Config
  raise : ConfigError -> Eff b ['config ::: EXCEPTION ConfigError]
  raise err = 'config :- Exception.raise err

-- --------------------------------------------------------------------- [ EOF ]
