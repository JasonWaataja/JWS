module Main where

import qualified Control.Monad as M
import qualified Data.List as L
import Data.Yaml as Y
import qualified JWS.Config as C
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as P

isImageFile :: FilePath -> Bool
isImageFile path = L.isSuffixOf "jpg" path || L.isSuffixOf "png" path

readImageFiles :: FilePath -> IO [FilePath]
readImageFiles path =
  Dir.listDirectory path
    >>= (M.msum . map readImageFiles . L.sort . L.filter isImageFile)

backgroundModeToFehArg :: C.BackgroundMode -> String
backgroundModeToFehArg mode =
  case mode of
    C.BackgroundFill -> "--bg-fill"
    C.BackgroundCenter -> "--bg-center"
    C.BackgroundMax -> "--bg-max"
    C.BackgroundScale -> "--bg-scale"
    C.BackgroundTile -> "--bg-tile"

setBackground :: FilePath -> C.Config -> IO Bool
setBackground path config =
  do
    (_, _, _, handle) <-
      P.createProcess $
        P.proc
          "feh"
          [ backgroundModeToFehArg $ C.configBackgroundMode config,
            "--image-bg",
            C.configBackgroundColor config,
            path
          ]
    exitCode <- P.waitForProcess handle
    return (exitCode == Exit.ExitSuccess)

main :: IO ()
main =
  do
    result <- C.configFromFile "/home/jason/.jws.yaml"
    case result of
      Left e -> putStrLn $ Y.prettyPrintParseException e
      Right c -> do
        setBackground "/home/jason/Pictures/wallpapers/kill_la_kill/ryuuko_satsuki_sketch.png" c
        return ()
