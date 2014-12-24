module Main

import System

import Effects
import Effect.File
import Effect.StdIO

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Data.SortedMap

import Config.JSON
import Config.YAML
import COnfig.INI

configMain : String -> {[STDIO, FILE_IO ()]} Eff ()
configMain fname = with Effects do
    d <- readINIConfig fname
    case d of
      Left err  => putStrLn $ err
      Right res => do
        putStrLn $ show res

main : IO ()
main = do
    args <- getArgs
    case (processArgs args) of
      Just f  => run $ configMain f
      Nothing => putStrLn "Wrong"
  where
    processArgs : List String -> Maybe String
    processArgs [x]       = Nothing
    processArgs (x::y::z) = Just y
