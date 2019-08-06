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
  connector (run mpvHandle)

getConnector :: Opts.Options -> (Handle -> IO ()) -> IO ()
getConnector o act = case Opts.opMode o of
  Opts.Serve -> TCP.listen TCP.HostAny port (\(s, _) -> TCP.accept s actor)
  Opts.Connect host -> TCP.connect host port actor
  where
    port = show (Opts.port o)
    actor (s, _) = act =<< Sock.socketToHandle s ReadWriteMode

getSockHandle :: FilePath -> IO Handle
getSockHandle p = do
  s <- Sock.socket Sock.AF_UNIX Sock.Stream 0
  Sock.connect s (Sock.SockAddrUnix p)
  Sock.socketToHandle s ReadWriteMode

-- TODO: Newtype wrappers around Handle
run :: Handle -> Handle -> IO ()
run mpv tcp = do
  disableEvents mpv
  _ <- forkIO $ forever getFromThem
  forever (mapM_ (tellThem . BS.pack . show) =<< getPlayTime mpv <* pause)
  where
    getFromThem = do
      msg <- BS.hGetLine tcp
      putStrLn ("Them: " ++ show msg)
    tellThem msg = do
      putStrLn ("Us: " ++ show msg)
      BS.hPutStrLn tcp msg
    pause = threadDelay 1e6

disableEvents :: Handle -> IO ()
disableEvents h = do
  BS.hPutStrLn h "{\"command\":[\"disable_event\",\"all\"]}"
  waitForSuccess
  where
    waitForSuccess = do
      json <- JSON.decodeStrict' <$> BS.hGetLine h
      let errorValue = JSON.parseMaybe (.: "error") =<< json :: Maybe String
      unless (errorValue == Just "success") waitForSuccess

getPlayTime :: Handle -> IO (Maybe Double)
getPlayTime h = do
  BS.hPutStrLn h "{\"command\":[\"get_property\",\"playback-time\"]}"
  (JSON.parseMaybe (.: "data") <=< JSON.decodeStrict') <$> BS.hGetLine h
