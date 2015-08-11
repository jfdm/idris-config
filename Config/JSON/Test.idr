-- ---------------------------------------------------------------- [ Test.idr ]
-- Module    : Test.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Config.JSON.Test

import Config.JSON

import Test.Parsing

jsonTest1 : IO ()
jsonTest1 =
    canParse
      Nothing
      parseJSONFile
      "{\n    \"firstName\": \"John\",\n    \"lastName\": \"Smith\",\n    \"isAlive\": true,\n    \"age\": 25,\n    \"height_cm\": 167.6,\n    \"address\": {\n        \"streetAddress\": \"21 2nd Street\",\n        \"city\": \"New York\",\n        \"state\": \"NY\",\n        \"postalCode\": \"10021-3100\"\n    },\n    \"phoneNumbers\": [\n        {\n            \"type\": \"home\",\n            \"number\": \"212 555-1234\"\n        },\n        {\n            \"type\": \"office\",\n            \"number\": \"646 555-4567\"\n        }\n    ],\n    \"children\": [],\n    \"spouse\": null\n}\n"


runTests : IO ()
runTests = do
    jsonTest1


-- --------------------------------------------------------------------- [ EOF ]
