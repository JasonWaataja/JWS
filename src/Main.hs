-- | Main module for JWS.
module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Suspend as Suspend
import qualified Control.Concurrent.Timer as Timer
import qualified Control.Monad.Loops as Loops
import qualified DBus as DB
import qualified DBus.Client as DBClient
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Yaml as Y
import qualified HSH.ShellEquivs as SE
import qualified JWS.Config as C
import qualified JWS.Options as O
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Process as P
import qualified System.Random as Random

-- | @'isImageFile' path@ returns whether @path@ represents an image file.
isImageFile :: FilePath -> Bool
isImageFile path = L.isSuffixOf "jpg" path || L.isSuffixOf "png" path

-- | @'readImageFiles' path@ reads all image files at @path@ or any subdirectory
-- recursively and returns a list of the results. The results are is if one
-- walked the file tree going through the contents of each directory in
-- alphabetical order.
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

-- | @'backgroundModeToFehArg' mode@ returns a string that may be passed as an
-- option to @feh@ to control the mode used to display the background on the
-- screen.
backgroundModeToFehArg :: C.BackgroundMode -> String
backgroundModeToFehArg mode =
  case mode of
    C.BackgroundFill -> "--bg-fill"
    C.BackgroundCenter -> "--bg-center"
    C.BackgroundMax -> "--bg-max"
    C.BackgroundScale -> "--bg-scale"
    C.BackgroundTile -> "--bg-tile"

-- | @'setBackground' path config@ sets the background to the image at @path@
-- using the mode specified in @config@. Returns true on success and false on
-- failure.
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

-- | @'expandPath' path@ performs shell globbing expansion on @path@ to expand
-- @path@. If @path@ would expand to more than one path, returns one of them.
expandPath :: String -> IO FilePath
expandPath path = head <$> SE.glob path

-- | @'fakeBackgroundList' config@ returns the list of all images specified in
-- the files and directories of @config@.
makeBackgroundList :: C.Config -> IO [FilePath]
makeBackgroundList config =
  do
    paths <- mapM expandPath (C.configFiles config)
    allFiles <- mapM readImageFiles paths
    return (concat allFiles)

-- | @'setRandomBackground' config gen@ chooses a random background from config
-- and sets it as the background using the options in the configuration.
setRandomBackground :: Random.RandomGen g => C.Config -> g -> IO g
setRandomBackground config gen =
  do
    backgrounds <- makeBackgroundList config
    let len = length backgrounds
        (index, newGen) = Random.randomR (0, len - 1) gen
        background = backgrounds !! index
     in do
          _ <- setBackground background config
          return newGen

-- | @'backgroundDelay' config@ pauses execution for the wait time specified in
-- @config@.
backgroundDelay :: C.Config -> IO ()
backgroundDelay config =
  Concurrent.threadDelay $ 1000000 * fromIntegral (C.configSwitchTime config)

data Message = Timer | Stop | Restart

-- -- | Shows all the backgrounds specified in the configuration in order, waiting
-- -- between each one. This function never returns, after it's shown all
-- -- backgrounds it starts over.
-- showAllBackgrounds :: C.Config -> IO ()
-- showAllBackgrounds config =
-- M.forever $ do
-- backgrounds <- makeBackgroundList config
-- mapM_
-- ( \background -> do
-- setBackground background config
-- backgroundDelay config
-- )
-- backgrounds

openClient :: IO (Maybe DBClient.Client)
openClient = do
  client <- DBClient.connectSession
  nameResult <- DBClient.requestName client (DB.busName_ "com.github.JWS") [DBClient.nameDoNotQueue]
  case nameResult of
    DBClient.NamePrimaryOwner -> return (Just client)
    DBClient.NameExists -> do
      putStrLn "JWS already running"
      DBClient.disconnect client
      return Nothing
    _ -> do
      DBClient.disconnect client
      return Nothing

showAllBackgrounds :: C.Config -> DBClient.Client -> Concurrent.Chan Message -> IO ()
showAllBackgrounds config client messageChan = do
  bgList <- makeBackgroundList config
  timer <- Timer.repeatedTimer (Concurrent.writeChan messageChan Timer) (Suspend.sDelay $ fromIntegral $ C.configSwitchTime config)
  loop (cycle bgList) timer
  where
    loop :: [FilePath] -> Timer.TimerIO -> IO ()
    loop (background : restBackgrounds) timer = do
      _ <- setBackground background config
      message <- Concurrent.readChan messageChan
      case message of
        Timer -> loop restBackgrounds timer
        Stop -> do
          cleanup timer
          return ()
        Restart -> do
          cleanup timer
          main
    loop _ _ = do error "Empty background list"
    cleanup :: Timer.TimerIO -> IO ()
    cleanup timer = do
      Timer.stopTimer timer
      DBClient.disconnect client

-- | Shows all backgrounds specified in the configuration in a randomized order,
-- waiting between each one. This function never returns, it continues to show
-- random backgrounds.
showBackgroundsRandomized :: C.Config -> IO ()
showBackgroundsRandomized config =
  do
    gen <- Random.getStdGen
    helper gen
  where
    helper :: Random.RandomGen g => g -> IO ()
    helper gen =
      do
        gen' <- setRandomBackground config gen
        backgroundDelay config
        helper gen'

-- | An action that returns the configuration file to use if one could be found,
-- or Nothing otherwise. Searches @$XDG_CONFIG_HOME/jws/config.yaml@ and
-- @~/.jws.yaml@ in that order.
getConfigPath :: O.RunOptions -> IO (Maybe FilePath)
getConfigPath options =
  case O.optionsConfigFile options of
    Just file -> return (Just file)
    Nothing -> do
      xdgConfigPath <- Dir.getXdgDirectory Dir.XdgConfig "jws/config.yaml"
      homeConfigPath <- expandPath "~/.jws.yaml"
      Loops.firstM Dir.doesFileExist [xdgConfigPath, homeConfigPath]

-- | Runs the main program with the given configuration.
runWithConfig :: C.Config -> IO ()
runWithConfig config
  | null (C.configFiles config) =
    Exit.die "no wallpapers listed in \"files\" in configuration"
  | C.configRotate config =
    if C.configRandomize config
      then showBackgroundsRandomized config
      else do
        client <- openClient
        messageChan <- Concurrent.newChan
        case client of
          Just c -> showAllBackgrounds config c messageChan
          Nothing -> return ()
  | C.configRandomize config = do
    gen <- Random.getStdGen
    _ <- setRandomBackground config gen
    return ()
  | otherwise = do
    backgrounds <- makeBackgroundList config
    _ <- setBackground (head backgrounds) config
    return ()

-- | Takes a configuration and returns a new configuration with the same values
-- but with any provided arguments from the options applied.
configWithOptions :: C.Config -> O.RunOptions -> C.Config
configWithOptions config options =
  C.Config
    { C.configRotate =
        (C.configRotate config || O.optionsRotate options)
          && not (O.optionsNoRotate options),
      C.configRandomize =
        (C.configRandomize config || O.optionsRandomize options)
          && not (O.optionsNoRandomize options),
      C.configSwitchTime =
        Maybe.fromMaybe
          (C.configSwitchTime config)
          (O.optionsTime options),
      C.configBackgroundMode =
        Maybe.fromMaybe (C.configBackgroundMode config) (O.optionsMode options),
      C.configBackgroundColor =
        Maybe.fromMaybe
          (C.configBackgroundColor config)
          (O.optionsBackgroundColor options),
      C.configFiles = C.configFiles config ++ O.optionsFiles options
    }

run :: O.RunOptions -> IO ()
run options = do
  configPath <- getConfigPath options
  do
    config <- case configPath of
      Nothing -> return C.defaultConfig
      Just path -> do
        parseResult <- C.configFromFile path
        case parseResult of
          Left e ->
            Exit.die $
              "error parsing config file: " ++ Y.prettyPrintParseException e
          Right config -> return config
    runWithConfig $ configWithOptions config options

-- | Executes JWS.
main :: IO ()
main = do
  options <- O.parseOptions
  case options of
    O.Run runOpts -> run runOpts
    _ -> return ()
