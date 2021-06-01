module JWS.Options
  ( Options (..),
    parseOptions,
  )
where

import Data.Semigroup ((<>))
import qualified JWS.Config as C
import qualified Options.Applicative as O

data Options = Options
  { optionsConfigFile :: !(Maybe FilePath),
    optionsRotate :: !(Maybe Bool),
    optionsNoRotate :: !(Maybe Bool),
    optionsRandomize :: !(Maybe Bool),
    optionsNoRandomize :: !(Maybe Bool),
    optionsTime :: !(Maybe Integer),
    optionsMode :: !(Maybe C.BackgroundMode),
    optionsBackgroundColor :: !(Maybe String),
    optionsFiles :: ![FilePath]
  }
  deriving (Eq, Show)

parseBackgroundMode = O.maybeReader C.backgroundModeFromString

jwsParser :: O.Parser Options
jwsParser =
  Options
    <$> O.optional
      ( O.strOption
          ( O.long "config-file"
              <> O.short 'c'
              <> O.metavar "CONFIG_FILE"
              <> O.help "Specify configuration file"
          )
      )
    <*> O.optional
      ( O.switch
          ( O.long "rotate-image"
              <> O.short 'r'
              <> O.help "Cycle through images"
          )
      )
    <*> O.optional
      ( O.switch
          ( O.long "no-rotate-image"
              <> O.help "Don't cycle through images"
          )
      )
    <*> O.optional
      ( O.switch
          ( O.long "randomize-order"
              <> O.short 's'
              <> O.help "Display backgrounds in a random order"
          )
      )
    <*> O.optional
      ( O.switch
          ( O.long "no-randomize-order"
              <> O.help "Don't display backgrounds in a random order"
          )
      )
    <*> O.optional
      ( O.option
          O.auto
          ( O.long "time"
              <> O.short 't'
              <> O.help "Time between switching backgroudns"
          )
      )
    <*> O.optional
      ( O.option
          parseBackgroundMode
          ( O.long "mode"
              <> O.short 'm'
              <> O.metavar "BACKGROUND_MODE"
              <> O.help "The mode for displaying backgrounds"
          )
      )
    <*> O.optional
      ( O.strOption
          ( O.long "bg-color"
              <> O.short 'b'
              <> O.metavar "COLOR"
              <> O.help "Background color for extra space"
          )
      )
    <*> O.many (O.argument O.str (O.metavar "BACKGROUND_FILES..."))

jwsParserInfo :: O.ParserInfo Options
jwsParserInfo =
  O.info
    (O.helper <*> jwsParser)
    ( O.fullDesc
        <> O.progDesc
          "Uses feh to set a wallpaper and optionally change it at\
          \ a regular interval.  Reads config files such as ~/.jws and\
          \ $XDG_CONFIG_HOME/jws. Can be used with a set of files and\
          \ directories and options to customize your desktop's wallpaper\
          \ rotation."
        <> O.header "jws - A wallpaper setter for minimalist window managers"
    )

parseOptions :: IO Options
parseOptions = O.execParser jwsParserInfo
