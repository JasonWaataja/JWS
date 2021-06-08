{-# LANGUAGE OverloadedStrings #-}

-- | Configuration for JWS.
module JWS.Config
  ( BackgroundMode (..),
    backgroundModeFromString,
    Config (..),
    defaultConfig,
    configFromFile,
  )
where

import qualified Data.Aeson.Types as ATypes
import qualified Data.Text as T
import Data.Yaml ((.!=), (.:?))
import qualified Data.Yaml as Y

-- | The mode used to decide how an image is stretched to display on a screen.
-- Corresponds to feh's background options.
data BackgroundMode
  = BackgroundFill
  | BackgroundCenter
  | BackgroundMax
  | BackgroundScale
  | BackgroundTile
  deriving (Eq, Show)

-- Parses a simple background mode String that can be "fill", "center", "max",
-- "scale", or "tile". Returns the background mode if the input matched one of
-- these and Nothing otherwise.
backgroundModeFromString :: String -> Maybe BackgroundMode
backgroundModeFromString s
  | s == "fill" = Just BackgroundFill
  | s == "center" = Just BackgroundCenter
  | s == "max" = Just BackgroundMax
  | s == "scale" = Just BackgroundScale
  | s == "tile" = Just BackgroundTile
  | otherwise = Nothing

instance Y.FromJSON BackgroundMode where
  parseJSON =
    Y.withText "BackgroundMode" $ \v ->
      case backgroundModeFromString $ T.unpack v of
        Just mode -> pure mode
        Nothing -> ATypes.parseFail ("unknown background mode " ++ T.unpack v)

-- | A configuration for JWS.
data Config = Config
  { -- | Whether or not to switch images or stay on a single image.
    configRotate :: !Bool,
    -- | If rotating backgrounds, whether or not to randomize the order.
    configRandomize :: !Bool,
    -- | If rotating background, the number of seconds to wait between
    -- switching.
    configSwitchTime :: !Integer,
    -- | The mode to use to fit images to the screen.
    configBackgroundMode :: !BackgroundMode,
    -- | The color to use to fill in any blank space left after displaying the
    -- image on the screen.
    configBackgroundColor :: !String,
    -- | The list of files and directories of backgrounds.
    configFiles :: ![String]
  }
  deriving (Eq, Show)

-- | The default configuration for JWS.
defaultConfig :: Config
defaultConfig =
  Config
    { configRotate = False,
      configRandomize = False,
      configSwitchTime = 600,
      configBackgroundMode = BackgroundFill,
      configBackgroundColor = "0x000000",
      configFiles = []
    }

instance Y.FromJSON Config where
  parseJSON =
    Y.withObject "Config" $ \v ->
      Config
        <$> (v .:? "rotate" .!= configRotate defaultConfig)
        <*> (v .:? "randomize" .!= configRandomize defaultConfig)
        <*> (v .:? "switch_time" .!= configSwitchTime defaultConfig)
        <*> (v .:? "background_mode" .!= configBackgroundMode defaultConfig)
        <*> (v .:? "background_color" .!= configBackgroundColor defaultConfig)
        <*> (v .:? "files" .!= configFiles defaultConfig)

-- | @'configFromFile' path@ reads the file at @path@ and returns the
-- stored configuration file, or a YAML exception.
configFromFile :: FilePath -> IO (Either Y.ParseException Config)
configFromFile = Y.decodeFileEither
