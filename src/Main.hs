{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS
import Data.Fixed (divMod')
import Data.IORef
import qualified Network.Socket as Sock
import qualified Network.Simple.TCP as TCP
import System.IO
import Text.Printf
import Text.Read

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
  theirRef <- commTcp tcp
  ourRef <- commMpv mpv tcp
  forever $ do
    showTimes ourRef theirRef
    threadDelay 0.5e6

disableEvents :: Handle -> IO ()
disableEvents h = do
  BS.hPutStrLn h "{\"command\":[\"disable_event\",\"all\"]}"
  waitForSuccess
  where
    waitForSuccess = do
      json <- JSON.decodeStrict' <$> BS.hGetLine h
      let errorValue = JSON.parseMaybe (.: "error") =<< json :: Maybe String
      unless (errorValue == Just "success") waitForSuccess

commTcp :: Handle -> IO (IORef (Maybe Double))
commTcp tcp = do
  ref <- newIORef Nothing
  _ <- forkIO $ forever (getTime ref)
  pure ref
  where
    getTime r = writeIORef r . readMaybe . BS.unpack =<< BS.hGetLine tcp

commMpv :: Handle -> Handle -> IO (IORef (Maybe Double))
commMpv mpv tcp = do
  ref <- newIORef Nothing
  _ <- forkIO $ forever (broadcastTime ref)
  pure ref
  where
    broadcastTime r = do
      time <- getPlayTime mpv
      writeIORef r time
      mapM_ (BS.hPutStrLn tcp . BS.pack . show) time
      threadDelay 1e6

getPlayTime :: Handle -> IO (Maybe Double)
getPlayTime h = do
  BS.hPutStrLn h "{\"command\":[\"get_property\",\"playback-time\"]}"
  (JSON.parseMaybe (.: "data") <=< JSON.decodeStrict') <$> BS.hGetLine h

showTimes :: IORef (Maybe Double) -> IORef (Maybe Double) -> IO ()
showTimes ourRef theirRef = do
  ours <- readIORef ourRef
  theirs <- readIORef theirRef
  let diff = (-) <$> ours <*> theirs
  printf "\rUs: %s  Them: %s  Diff:%s  "
         (renderTime ours) (renderTime theirs) (renderDiff diff)
  hFlush stdout
  where
    renderTime Nothing = "--:--" :: String
    renderTime (Just t) = printf "%02d:%02.0f" (m :: Int) s
      where (m, s) = divMod' (max 0 t) 60
    renderDiff Nothing = " ---" :: String
    renderDiff (Just d) = printf "%+4.0f" d
