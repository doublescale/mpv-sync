{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options
  ( Options(..)
  , OpMode(..)
  , get
  ) where

import Control.Applicative
import qualified Options.Applicative as O

data Options = Options
  { sockPath :: FilePath
  , port :: Int
  , opMode :: OpMode
  }

data OpMode = Serve | Connect String

parseOptions :: O.Parser Options
parseOptions = do
  sockPath <- O.strArgument (O.metavar "MPVSOCKET"
                             <> O.help "mpv's --input-ipc-server")
  port <- O.option O.auto (O.short 'p' <> O.long "port" <> O.metavar "PORT"
                           <> O.help "TCP port to serve on / connect to")
  opMode <- parseOpMode
  pure Options{..}

parseOpMode :: O.Parser OpMode
parseOpMode =
  ( O.flag' Serve (O.short 's' <> O.long "serve"
                   <> O.help "Listen for connections")
  ) <|> Connect <$>
  ( O.option O.str (O.short 'c' <> O.long "connect" <> O.metavar "HOST"
                    <> O.help "Connect to a server")
  )

parserInfo :: O.ParserInfo Options
parserInfo =
  O.info (parseOptions <**> O.helper)
         (O.fullDesc <> O.header "Exchange mpv playtimes.")

get :: IO Options
get = O.execParser parserInfo
