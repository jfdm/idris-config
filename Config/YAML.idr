-- ---------------------------------------------------------------- [ YAML.idr ]
-- Description : Parse YAML files.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.YAML

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Data.HVect
import Data.SortedMap

import Config.Parse.Common
import Config.Parse.Utils
import Config.Parse.Reader

%access private

-- ------------------------------------------------------------------- [ Model ]
public
data YAMLTag = TagStr | TagMap | TagSeq | TagNull | TagBool | TagInt | TagFloating

instance Show YAMLTag where
  show TagStr = "!!str"
  show TagMap = "!!map"
  show TagSeq = "!!seq"
  show TagBool = "!!bool"
  show TagInt = "!!int"
  show TagFloat = "!!float"


-- complex maps not supported
public
data YAMLNode : Type where
  YAMLLiteral : (tag : Maybe String) -> (anchor : Maybe String) -> String -> YAMLNode
  YAMLScalar  : (tag : Maybe String) -> (anchor : Maybe String) -> String -> YAMLNode
  YAMLSeq     : (tag : Maybe String) -> (anchor : Maybe String) -> List YAMLNode -> YAMLNode
  YAMLMap     : (tag : Maybe String) -> (anchor : Maybe String) -> SortedMap String (YAMLNode) -> YAMLNode
  YAMLBlock   : (tag : Maybe String) -> (anchor : Maybe String) -> String -> YAMLNode -> YAMLNode

{-
% Many Directives
--- # Begin Document


... # End DOcument
-}


{-
Alias nodes are: &<label> => def *<label>
Empty Nodes are empty
Scalars are: SQuote, DQuote, Raw
Collections:
  Seq are denoted by []
  Maps are denoted by {(key :value)+}
Blocks are Either Literal (|) or Folded (>)
 With Indicator Indent 1
 With Indicator Chomp
   Keep  +
   Clip Default
   Strip -
-}

public
data JsonValue = JsonString String
               | JsonNumber Float
               | JsonBool Bool
               | JsonNull
               | JsonArray (List JsonValue)
               | JsonObject (SortedMap String JsonValue)

-- --------------------------------------------------------------------- [ EOF ]
