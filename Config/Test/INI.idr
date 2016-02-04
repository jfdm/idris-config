-- ----------------------------------------------------------------- [ INI.idr ]
-- Module    : INI.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Test.INI

import Config.INI

import Test.Parsing

%access export

-- ------------------------------------------------------------------- [ BEGIN ]

runTests : IO ()
runTests = do
    canParse (Just "INI Test 1") parseINI
        """; last modified 1 April 2001 by John Doe
[owner]
name=John Doe
organization=Acme Widgets Inc.

[database]
; use IP address in case network name resolution is not working
server=192.0.2.62
port=143
file="payroll.dat"
        """

    canParse (Just "INI Test 2") parseINI
        """#Comment
a=b
a=c

[section]
a=b

# comment

[section]

b=b
"""

-- --------------------------------------------------------------------- [ EOF ]
