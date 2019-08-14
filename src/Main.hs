{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
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

type PlayState = Maybe Double

main :: IO ()
main = do
  o <- Opts.get
  ourState <- newIORef Nothing
  theirState <- newIORef Nothing
  fromMpvChan <- newChan
  fromTcpChan <- newChan
  toTcpChan <- newChan
  _ <- forkIO $ retrying $ do
    mpvHandle <- getSockHandle (Opts.sockPath o)
    putStrLn "\n* Got mpv handle"
    disableEvents mpvHandle
    forever $ do
      time <- getPlayTime mpvHandle
      writeChan fromMpvChan time
      threadDelay 0.5e6
  _ <- forkIO $ retrying (getConnector o $ \tcpHandle -> do
    putStrLn "\n* Connected TCP"
    _ <- forkIO $ forever $
      writeChan fromTcpChan . readMaybe . BS.unpack =<< BS.hGetLine tcpHandle
    forever $
      mapM_ (BS.hPutStrLn tcpHandle . BS.pack . show) =<< readChan toTcpChan
    )
  _ <- forkIO $ forever $ do
    mpvMsg <- readChan fromMpvChan
    writeIORef ourState mpvMsg
    writeChan toTcpChan mpvMsg
    -- TODO: Update semaphore for rendering?
  _ <- forkIO $ forever $ do
    tcpMsg <- readChan fromTcpChan
    writeIORef theirState tcpMsg
    -- TODO: Update semaphore for rendering?
  forever $ do
    join $ showPlayStates <$> readIORef ourState <*> readIORef theirState
    threadDelay 0.5e6 -- TODO: Replace by semaphore

retrying :: IO () -> IO ()
retrying act =
  forever $ do
    act `catch` \(_ :: IOException) -> pure ()
    threadDelay 1e6

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

showPlayStates :: PlayState -> PlayState -> IO ()
showPlayStates ours theirs = do
  let diff = (-) <$> ours <*> theirs
  printf "\rUs: %s  Them: %s  Diff:%s  "
         (renderTime ours) (renderTime theirs) (renderDiff diff)
  hFlush stdout
  where
    renderTime Nothing = "--:--" :: String
    renderTime (Just t) = printf "%02d:%02d" (m :: Int) (floor s :: Int)
      where (m, s) = divMod' (max 0 t) 60
    renderDiff Nothing = " ---" :: String
    renderDiff (Just d) = printf "%+4.0f" d
