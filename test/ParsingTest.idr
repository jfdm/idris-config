module ParsingTest

import System
import Effects
import Effect.Stdio
import Effect.Exception
import Effect.System

import public Control.Monad.Identity

import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import TestRunner

-- ----------------------------------------------------------- [ Parsing Tests ]

||| Expect a parsing test to pass.
parseTestGood : Parser a -> String -> Test
parseTestGood p s = case parse p s of
    Left err => raise $ "Expected passing test failed " ++ err
    Right re => pure ()

||| Expect a test to fail
parseTestBad : Parser a -> String -> Test
parseTestBad p s = case parse p s of
    Left err => pure ()
    Right re => raise "Expected failing test passed..."
-- --------------------------------------------------------------------- [ EPF ]
