module Test.Main

import System

import Test.TestRunner
import Test.ParsingTest

import Config.JSON
import Config.YAML
import Config.INI
import Config.Properties

-- --------------------------------------------------------------- [ INI Tests ]
iniTest : Test
iniTest = parseTestGood parseINI "# I am a comment\nas=bf\n[section] #sdsd\nas=fg\n"

iniTest1 : Test
iniTest1 = parseTestBad parseINI "# I am a comment\n[sec"

iniTest2 : Test
iniTest2 = parseTestGood parseINI "#Comment\na=b\na=c\n\n\n[section]\n\na=b\n\n\n\n# comment\n\n\n[section]\n\n\nb=b\n"

iniTest3 : Test
iniTest3 = parseTestGood parseINI "; last modified 1 April 2001 by John Doe\n[owner]\nname=John Doe\n\norganization=Acme Widgets Inc.\n\n[database]\n\n; use IP address in case network name resolution is not working\nserver=192.0.2.62\nport=143\nfile=\"payroll.dat\"\n"
-- -------------------------------------------------------------- [ Properties ]

propTest : Test
propTest = parseTestGood parseProperties "! I am comment\nwebsite = English\ntab : as\n"


-- -------------------------------------------------------------------- [ YAML ]
yamlTest1 : Test
yamlTest1 = parseTestGood parseYAMLDoc "%YAML 1.2\n---\nreceipt: Oz Ware Purchase Invoice\ndate: \"2012 08 06\"\ncustomer: {given: Dorothy, family: Gale}\n\nitems: {partno:   A4786, descrip:   Water Bucket Filled}\nitems: { partno:   E1628,\n      descrip:   High Heeled Ruby Slippers,\n      size:      8,\n      price:     100.27,\n\n      quantity:  1}\n\nbillto: { city: East Centerville, state: KS}\n\n...\n"

-- -------------------------------------------------------------------- [ JSON ]

jsonTest1 : Test
jsonTest1 = parseTestGood parseJSONFile "{\n    \"firstName\": \"John\",\n    \"lastName\": \"Smith\",\n    \"isAlive\": true,\n    \"age\": 25,\n    \"height_cm\": 167.6,\n    \"address\": {\n        \"streetAddress\": \"21 2nd Street\",\n        \"city\": \"New York\",\n        \"state\": \"NY\",\n        \"postalCode\": \"10021-3100\"\n    },\n    \"phoneNumbers\": [\n        {\n            \"type\": \"home\",\n            \"number\": \"212 555-1234\"\n        },\n        {\n            \"type\": \"office\",\n            \"number\": \"646 555-4567\"\n        }\n    ],\n    \"children\": [],\n    \"spouse\": null\n}\n"

-- -------------------------------------------------------------------- [ Main ]
runTests : IO ()
runTests = do
    run $ tests [ iniTest, iniTest1, iniTest2, iniTest3
                , propTest
                , yamlTest1
                , jsonTest1]

    exit 0

-- --------------------------------------------------------------------- [ EOF ]
