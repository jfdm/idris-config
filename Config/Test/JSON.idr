-- ---------------------------------------------------------------- [ Test.idr ]
-- Module    : Test.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Config.Test.JSON

import Config.JSON

import Test.Parsing

%access export

jsonTest1 : IO ()
jsonTest1 = canParse (Just "JSON Test 1") parseJSONFile
      """{
          "firstName": "John",
          "lastName": "Smith",
          "isAlive": true,
          "age": 25,
          "height_cm": 167.6,
          "address": {
              "streetAddress": "21 2nd Street",
              "city": "New York",
              "state": "NY",
              "postalCode": "10021-3100"
          },
          "phoneNumbers": [
              {
                  "type": "home",
                  "number": "212 555-1234"
              },
              {
                  "type": "office",
                  "number": "646 555-4567"
              }
          ],
          "children": [],
          "spouse": null
      }
      """

jsonTest2 : IO ()
jsonTest2 = canParse (Just "JSON Test 2") parseJSONFile
    """{
        "$schema": "http://json-schema.org/draft-03/schema#",
        "name": "Product",
        "type": "object",
        "properties": {
            "id": {
                "type": "number",
                "description": "Product identifier",
                "required": true
            },
            "name": {
                "type": "string",
                "description": "Name of the product",
                "required": true
            },
            "price": {
                "type": "number",
                "minimum": 0,
                "required": true
            },
            "tags": {
                "type": "array",
                "items": {
                    "type": "string"
                }
            },
            "stock": {
                "type": "object",
                "properties": {
                    "warehouse": {
                        "type": "number"
                    },
                    "retail": {
                        "type": "number"
                    }
                }
            }
        }
    }
    """
jsonTest3 : IO ()
jsonTest3 = canParse (Just "JSON Test 3") parseJSONFile
    """{
    "firstName": "John",
    "lastName": "Smith",
    "isAlive": true,
    "age": 25,
    "height_cm": 167.6,
    "address": {
        "streetAddress": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": "10021-3100"
    },
    "phoneNumbers": [
        {
            "type": "home",
            "number": "212 555-1234"
        },
        {
            "type": "office",
            "number": "646 555-4567"
        }
    ],
    "children": [],
    "spouse": null
    }
    """

runTests : IO ()
runTests = do
    jsonTest1
    jsonTest2
    jsonTest3

-- --------------------------------------------------------------------- [ EOF ]
