-- -------------------------------------------------------------- [ Common.idr ]
-- Description : Common Parsing Functions
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Parse.Common

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Config.Parse.Utils

%access public

langSpace : Monad m => ParserT m String () -> ParserT m String ()
langSpace p = p <|> space <?> "Space Lang"

keyvalue : String
         -> Parser String
         -> Parser (String, String)
keyvalue s value = do
    k <- word
    space
    token s
    v <- value
    space
    pure (k,v)
  <?> "KVPair"
-- --------------------------------------------------------------------- [ EOF ]
