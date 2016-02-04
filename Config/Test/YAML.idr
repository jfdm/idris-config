-- ---------------------------------------------------------------- [ YAML.idr ]
-- Module    : YAML.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Config.Test.YAML

import Config.YAML
import Test.Parsing

%access export
-- ------------------------------------------------------------------- [ Begin ]

yamlTest1 : IO ()
yamlTest1 = canParse (Just "YAML Test 1") parseYAMLDoc
    """%YAML 1.2
---
receipt: Oz Ware Purchase Invoice
date: "2012 08 06"
customer: {given: Dorothy, family: Gale}
items: {partno:   A4786, descrip:   Water Bucket Filled}
items: { partno:   E1628,
      descrip:   High Heeled Ruby Slippers,
      size:      8,
      price:     100.27,
      quantity:  1}
billto: { city: East Centerville, state: KS}
...
    """

yamlTest2 : IO ()
yamlTest2 = parseTestB (Just "YAML Test 2") parseYAMLDoc
    """# sequencer protocols for Laser eye surgery
---
- step:  &id001                  # defines anchor label &id001
    instrument:      Lasik 2000
    pulseEnergy:     5.4
    pulseDuration:   12
    repetition:      1000
    spotSize:        1mm

- step: &id002
    instrument:      Lasik 2000
    pulseEnergy:     5.0
    pulseDuration:   10
    repetition:      500
    spotSize:        2mm

- step: *id001                   # refers to the first step (with anchor &id001)
- step: *id002                   # refers to the second step
- step: *id001
- step: *id002
"""

yamlTest3 : IO ()
yamlTest3 = canParse (Just "YAML Test 3") parseYAMLDoc
    """%YAML 1.2
---
pattern: {problem: file.p, solution: file.p}
pattern: {problem: file.p, solution: file.p}
...
    """

runTests : IO ()
runTests = do
  yamlTest1
  yamlTest2
  yamlTest3
-- --------------------------------------------------------------------- [ EOF ]
