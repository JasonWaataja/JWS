module Main where

import Data.Yaml as Y
import qualified JWS.Config as C

main :: IO ()
main =
  do
    result <- C.configFromFile "/home/jason/.jws.yaml"
    case result of
      Left e -> putStrLn $ Y.prettyPrintParseException e
      Right c -> print c
