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
import qualified Network.Simple.TCP as TCP
import System.IO

import qualified Options as Opts

main :: IO ()
main = do
  o <- Opts.get
  let connector = getConnector o
  mpvHandle <- getSockHandle (Opts.sockPath o)
  connector (runWith mpvHandle)

getConnector :: Opts.Options -> (Sock.Socket -> IO ()) -> IO ()
getConnector o act = case Opts.opMode o of
  Opts.Serve -> TCP.listen TCP.HostAny port (\(s,_) -> TCP.accept s (act . fst))
  Opts.Connect host -> TCP.connect host port (act . fst)
  where
    port = show (Opts.port o)

runWith :: Handle -> Sock.Socket -> IO ()
runWith mpv s = do
  tcp <- Sock.socketToHandle s ReadWriteMode
  let
    getFromThem = do
      msg <- BS.hGetLine tcp
      putStrLn ("Them: " ++ show msg)
    tellThem msg = do
      putStrLn ("Us: " ++ show msg)
      BS.hPutStrLn tcp msg
  _ <- forkIO $ forever getFromThem
  _ <- forkIO $ forever (BS.hPutStrLn mpv timeQuery >> threadDelay 1e6)
  forever (mapM_ (tellThem . BS.pack . show) . parseData =<< BS.hGetLine mpv)

getSockHandle :: FilePath -> IO Handle
getSockHandle p = do
  s <- Sock.socket Sock.AF_UNIX Sock.Stream 0
  Sock.connect s (Sock.SockAddrUnix p)
  Sock.socketToHandle s ReadWriteMode

timeQuery :: BS.ByteString
timeQuery = "{\"command\": [\"get_property\", \"playback-time\"]}"

parseData :: BS.ByteString -> Maybe Double
parseData = JSON.parseMaybe (.: "data") <=< JSON.decodeStrict'
