{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WebSockets
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.HTTP.Types.Status (status200)
import Network.Wai.Application.Static

import WaiAppStatic.Types
import Data.Maybe

import System.IO
import Debug.Trace (trace)

import Data.Text (Text)
import Data.IORef

import Control.Monad (forever)
import Control.Exception.Safe
import Control.Concurrent (forkIO, threadDelay, killThread)

immediatePutStrLn :: String -> IO ()
immediatePutStrLn text = do
  putStrLn text
  hFlush stdout

immediatePutStr :: String -> IO ()
immediatePutStr text = do
  putStr text
  hFlush stdout

websocketsServerApp :: IORef Int -> WebSockets.ServerApp
websocketsServerApp ref pendingConnection = do
  count <- readIORef ref
  immediatePutStrLn $ "Websokets server responding!, count: " ++ show count
  hFlush stdout
  connection <- WebSockets.acceptRequest pendingConnection
  threadId <- forkIO $ sendPingLoop connection
  atomicModifyIORef ref (\count -> (count+1, ()))
  result <- try $ forever $ do
    message <- WebSockets.receiveData connection :: IO Text
    immediatePutStrLn $ show message
  case result of
    Left (err::SomeException) -> immediatePutStrLn ("error: " ++ show err)
    Right _ -> immediatePutStrLn "websocket connection closed successfully"
    _       -> immediatePutStrLn "???"
  atomicModifyIORef ref (\count -> (count-1, ()))
  killThread threadId
  
sendPingLoop :: WebSockets.Connection -> IO ()
sendPingLoop connection = do
  immediatePutStrLn "start pinging..."
  forever $ do
    threadDelay 1000000
    WebSockets.sendPing connection ("ping" :: Text)

normalServerApp :: Wai.Application
normalServerApp _ respond = do
  immediatePutStrLn "Normal server responding!"
  respond $ Wai.responseFile status200 [("Content-Type", "text/html")] "../client/index.html" Nothing

router :: IORef Int -> Wai.Application
router ref req
  | null path = mainApp ref req
  | otherwise = trace (show path) (staticServer req)
    where
      path = Wai.pathInfo req

mainApp :: IORef Int -> Wai.Application
mainApp ref = websocketsOr connectionOptions (websocketsServerApp ref) normalServerApp
  where
    connectionOptions = WebSockets.defaultConnectionOptions 

staticServer :: Wai.Application
staticServer = staticApp $ settings { ssIndices = indices }
  where
    settings = defaultWebAppSettings "../client"
    indices = fromJust $ toPieces ["index.html"]

main :: IO ()
main = do
  ref <- newIORef 0
  Warp.run 8080 $ router ref

