-- ---------------------------------------------------------------- [ Effs.idr ]
-- Module    : Effs.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Config.Effs

import public Effects
import public Effect.File

%access public

ConfigEffs : List EFFECT
ConfigEffs = [FILE_IO ()]

-- --------------------------------------------------------------------- [ EOF ]
