-- ---------------------------------------------------------------- [ Test.idr ]
-- Module    : Test.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Test.JSON

import Config.JSON

import Lightyear.Testing

%access export

jsonTest1 : TestReport
jsonTest1 = parseTest "JSON Test 1"
                      parseJSONFile
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

jsonTest2 : TestReport
jsonTest2 = parseTest "JSON Test 2"
                      parseJSONFile
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

jsonTest3 : TestReport
jsonTest3 = parseTest "JSON Test 3"
                      parseJSONFile
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
runTests = Testing.runTests [jsonTest1, jsonTest2, jsonTest3]

-- --------------------------------------------------------------------- [ EOF ]
