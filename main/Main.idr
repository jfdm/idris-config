module Main

import System

import public Effects
import public Effect.File
import public Effect.StdIO

import Config.JSON

configMain : String -> {[STDIO, FILE_IO ()]} Eff ()
configMain fname = with Effects do
    d <- readJSONConfig fname
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
