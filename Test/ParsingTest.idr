module Test.ParsingTest

import System

import Effects
import Effect.StdIO
import Effect.Exception
import Effect.System

import public Lightyear
import public Lightyear.Strings

import Test.TestRunner

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
