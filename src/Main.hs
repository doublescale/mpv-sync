{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Network.Socket as Sock
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> runWith path
    _ -> do
      putStrLn "I need a Unix socket."
      exitFailure

runWith :: FilePath -> IO ()
runWith p = do
  h <- getSockHandle p
  _ <- forkIO $ forever (BS.hPutStrLn h timeQuery >> threadDelay 1e6)
  forever (mapM_ print . parseData =<< BS.hGetLine h)

getSockHandle :: FilePath -> IO Handle
getSockHandle p = do
  s <- Sock.socket Sock.AF_UNIX Sock.Stream 0
  Sock.connect s (Sock.SockAddrUnix p)
  Sock.socketToHandle s ReadWriteMode

timeQuery :: BS.ByteString
timeQuery = "{\"command\": [\"get_property\", \"playback-time\"]}"

parseData :: BS.ByteString -> Maybe Double
parseData = JSON.parseMaybe (.: "data") <=< JSON.decodeStrict'
