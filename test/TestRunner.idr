-- ---------------------------------------------------------- [ TestRunner.idr ]
-- Module      : TestRunner
-- Description : Defines and runs test.x
--
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module TestRunner

import public Effects
import public Effect.StdIO
import public Effect.Exception

-- --------------------------------------------------- [ Type Sigs and Aliases ]
TestEffs : List EFFECT
TestEffs = [STDIO, EXCEPTION String]

Test : Type
Test = {TestEffs} Eff ()

-- ------------------------------------------------------------- [ Test Runner ]
tests : List Test -> {TestEffs} Eff ()
tests Nil = do
    putStrLn "All tests passed"
    pure ()
tests (t::ts) = do
    result <- t
    tests ts
-- --------------------------------------------------------------------- [ EOF ]
