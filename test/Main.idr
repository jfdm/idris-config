module Main

import System

import TestRunner
import ParsingTest

import Config.JSON
import Config.YAML
import Config.INI
import Config.Properties

-- --------------------------------------------------------------- [ INI Tests ]
iniTest : Test
iniTest = parseTestGood parseINI "# I am a comment\nas=bf\n[section] #sdsd\nas=fg\n"

iniTest1 : Test
iniTest1 = parseTestBad parseINI "# I am a comment\n[sec"

-- -------------------------------------------------------------- [ Properties ]

propTest : Test
propTest = parseTestGood parseProperties "! I am comment\nwebsite = English\ntab : as\n"


-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests [iniTest, iniTest1]
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
