{-# LANGUAGE OverloadedStrings #-}

-- | Main module for JWS.
module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Suspend as Suspend
import qualified Control.Concurrent.Timer as Timer
import qualified Control.Exception as E
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
import qualified System.IO as IO
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
        (index, gen') = Random.randomR (0, len - 1) gen
        background = backgrounds !! index
     in do
          _ <- setBackground background config
          return gen'

dbusBusName :: DB.BusName
dbusBusName = "com.github.JWS"

dbusObjectPath :: DB.ObjectPath
dbusObjectPath = "/com/github/JWS/app"

dbusInterfaceName :: DB.InterfaceName
dbusInterfaceName = "com.github.JWS"

dbusStopMethod :: DB.MemberName
dbusStopMethod = "Stop"

dbusRestartMethod :: DB.MemberName
dbusRestartMethod = "Restart"

exportMethods :: DBClient.Client -> Concurrent.Chan Message -> IO ()
exportMethods client messageChan =
  DBClient.export
    client
    dbusObjectPath
    DBClient.defaultInterface
      { DBClient.interfaceName = dbusInterfaceName,
        DBClient.interfaceMethods =
          [ DBClient.autoMethod
              dbusStopMethod
              (Concurrent.writeChan messageChan MessageStop),
            DBClient.autoMethod
              dbusRestartMethod
              (Concurrent.writeChan messageChan MessageRestart)
          ]
      }

data Message = MessageTimer | MessageStop | MessageRestart

-- | Requests the unique bus name for JWS. Returns True if the name was
-- successfully acquired and False otherwise. Prints a message of the name was
-- already in use.
requestUniqueName :: DBClient.Client -> IO Bool
requestUniqueName client = do
  nameResult <-
    DBClient.requestName
      client
      dbusBusName
      [DBClient.nameDoNotQueue]
  case nameResult of
    DBClient.NamePrimaryOwner -> return True
    DBClient.NameExists -> do
      putStrLn "JWS already running"
      return False
    _ -> return False

withTimer :: Suspend.Delay -> a -> Concurrent.Chan a -> IO b -> IO b
withTimer delay message chan action =
  E.bracket
    (Timer.repeatedTimer (Concurrent.writeChan chan message) delay)
    Timer.stopTimer
    (const action)

data Result = ResultStop | ResultRestart deriving (Eq, Show)

-- | Shows all backgrounds specified in the configuration on loop. When all
-- backgrounds have been displayed, restarts from the beginning. If another
-- instance os JWS requests the main instance to stop, then closes the given
-- client.
showAllBackgrounds :: C.Config -> Concurrent.Chan Message -> IO Result
showAllBackgrounds config chan = do
  withTimer
    (Suspend.sDelay $ fromIntegral $ C.configSwitchTime config)
    MessageTimer
    chan
    (makeBackgroundList config >>= loop . cycle)
  where
    loop :: [FilePath] -> IO Result
    loop [] = error "empty background list"
    loop (background : backgrounds) = do
      _ <- setBackground background config
      message <- Concurrent.readChan chan
      case message of
        MessageTimer -> loop backgrounds
        MessageStop -> return ResultStop
        MessageRestart -> return ResultRestart

-- Shows randomized backgrounds specified in the configuration on loop. If
-- another instance os JWS requests the main instance to stop, then closes the
-- given client.
showBackgroundsRandomized :: C.Config -> Concurrent.Chan Message -> IO Result
showBackgroundsRandomized config chan = do
  gen <- Random.newStdGen
  withTimer
    (Suspend.sDelay $ fromIntegral $ C.configSwitchTime config)
    MessageTimer
    chan
    (loop gen)
  where
    loop :: Random.RandomGen g => g -> IO Result
    loop gen = do
      backgrounds <- makeBackgroundList config
      let (index, gen') = Random.randomR (0, length backgrounds - 1) gen
       in do
            _ <- setBackground (backgrounds !! index) config
            message <- Concurrent.readChan chan
            case message of
              MessageTimer -> loop gen'
              MessageStop -> return ResultStop
              MessageRestart -> return ResultRestart

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

runWithRotation :: C.Config -> IO Result
runWithRotation config =
  E.bracket
    DBClient.connectSession
    DBClient.disconnect
    ( \client -> do
        isPrimary <- requestUniqueName client
        if isPrimary
          then do
            chan <- Concurrent.newChan
            exportMethods client chan
            if C.configRandomize config
              then showBackgroundsRandomized config chan
              else showAllBackgrounds config chan
          else return ResultStop
    )

-- | Runs the main program with the given configuration.
runWithConfig :: C.Config -> IO ()
runWithConfig config
  | null (C.configFiles config) =
    Exit.die "no wallpapers listed in \"files\" in configuration"
  | C.configRotate config = do
    result <- runWithRotation config
    case result of
      ResultStop -> return ()
      ResultRestart -> main
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

-- | Runs JWS in normal mode to display one or more backgrounds.
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

-- | Sends the given member name to the active JWS instance. If an error is
-- encountered, prints a message.
sendDBusMessage :: DB.MemberName -> IO ()
sendDBusMessage message = do
  result <- E.bracket DBClient.connectSession DBClient.disconnect $ \client ->
    DBClient.call
      client
      ( DB.methodCall
          dbusObjectPath
          dbusInterfaceName
          message
      )
        { DB.methodCallDestination = Just dbusBusName
        }
  case result of
    Left err -> IO.hPutStrLn IO.stderr $ DB.methodErrorMessage err
    Right _ -> return ()

-- | Executes JWS.
main :: IO ()
main = do
  options <- O.parseOptions
  case options of
    O.Run runOpts -> run runOpts
    O.Stop -> sendDBusMessage dbusStopMethod
    O.Restart -> sendDBusMessage dbusRestartMethod
