module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.Yaml as Y
import qualified HSH.ShellEquivs as SE
import qualified JWS.Config as C
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as P
import qualified System.Random as Random

isImageFile :: FilePath -> Bool
isImageFile path = L.isSuffixOf "jpg" path || L.isSuffixOf "png" path

readImageFiles :: FilePath -> IO [FilePath]
readImageFiles path =
  do
    isFile <- Dir.doesFileExist path
    if isFile
      then
        if isImageFile path
          then return [path]
          else return []
      else do
        isDir <- Dir.doesDirectoryExist path
        if isDir
          then do
            contents <- Dir.listDirectory path
            files <- mapM (readImageFiles . (path </>)) (L.sort contents)
            return (concat files)
          else return []

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

-- | Performs tilde expansion
expandPath :: String -> IO FilePath
expandPath path = head <$> SE.glob path

makeBackgroundList :: C.Config -> IO [FilePath]
makeBackgroundList config =
  do
    paths <- mapM expandPath (C.configFiles config)
    allFiles <- mapM readImageFiles paths
    return (concat allFiles)

backgroundDelay :: C.Config -> IO ()
backgroundDelay config =
  Concurrent.threadDelay $ 1000000 * fromIntegral (C.configSwitchTime config)

showAllBackgrounds :: C.Config -> IO ()
showAllBackgrounds config =
  M.forever $ do
    backgrounds <- makeBackgroundList config
    mapM_
      ( \background -> do
          setBackground background config
          backgroundDelay config
      )
      backgrounds

showBackgroundsRandomized :: C.Config -> IO ()
showBackgroundsRandomized config =
  M.forever $ do
    gen <- Random.newStdGen
    backgrounds <- makeBackgroundList config
    let len = length backgrounds
        (index, _) = Random.randomR (0, len - 1) gen
        background = backgrounds !! index
     in do
       setBackground background config
       backgroundDelay config

-- TODO; Change this to firstM in the monad loops library.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM pred [] = return Nothing
findM pred (x : xs) =
  do
    isSatisfied <- pred x
    if isSatisfied
      then return (Just x)
      else findM pred xs

getConfigPath :: IO (Maybe FilePath)
getConfigPath =
  do
    xdgConfigPath <- Dir.getXdgDirectory Dir.XdgConfig "jws/config.yaml"
    homeConfigPath <- expandPath "~/.jws.yaml"
    findM Dir.doesFileExist [xdgConfigPath, homeConfigPath]

runWithConfig :: C.Config -> IO ()
runWithConfig config
  | null (C.configFiles config) =
    Exit.die "no wallpapers listed in \"files\" in configuration"
  | C.configRotate config =
    if C.configRandomize config
      then showBackgroundsRandomized config
      else showAllBackgrounds config
  | otherwise = do
    setBackground (head $ C.configFiles config) config
    return ()

main :: IO ()
main =
  do
    configPath <- getConfigPath
    case configPath of
      Nothing -> Exit.die "can't open configuration file"
      Just path -> do
        parseResult <- C.configFromFile path
        case parseResult of
          Left e -> Exit.die $ Y.prettyPrintParseException e
          Right config -> runWithConfig config
