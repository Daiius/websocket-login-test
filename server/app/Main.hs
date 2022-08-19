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

import Debug.Trace (trace)

import Data.Text (Text, unpack)

import Control.Monad (forever)
import Control.Exception.Safe (try, SomeException)

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)

import Utils (immediatePutStrLn, immediatePrint)
import Connections
import Messages (handleMessage)


instance Show WebSockets.Connection where
  show conn = "conn."


websocketsServerApp :: IORef ClientList -> WebSockets.ServerApp
websocketsServerApp ref pendingConnection = do
  immediatePutStrLn "Websokets server responding!"
  identity <- newIORef ""
  connection <- WebSockets.acceptRequest pendingConnection
  WebSockets.withPingThread connection 30 doNothing $ do
    result <- try $ forever $ do
      immediatePutStrLn "Start message loop..."
      message <- WebSockets.receiveData connection :: IO Text
      immediatePutStrLn "Got message..."
      immediatePutStrLn $ "\t" ++ unpack message
      handleMessage ref identity connection message
      immediatePutStrLn "Current status..."
      clientList <- readIORef ref
      immediatePrint clientList
    case result of
      Left (err::SomeException) -> immediatePutStrLn ("error: " ++ show err)
      Right _ -> immediatePutStrLn "websocket connection closed successfully"
    disconnectHelper identity
    where
      disconnectHelper identity = do
        idToDisconnect <- readIORef identity
        atomicModifyIORef ref (\l -> (removeClient l idToDisconnect, ()))
        immediatePutStrLn $ "message loop for " ++ unpack idToDisconnect ++ " end."
      doNothing = return ()
        
normalServerApp :: Wai.Application
normalServerApp _ respond = do
  immediatePutStrLn "Normal server responding!"
  respond $ Wai.responseFile status200 [("Content-Type", "text/html")] "../client/index.html" Nothing


router :: IORef ClientList -> Wai.Application
router ref req
  | null path = mainApp ref req
  | otherwise = trace (show path) (staticServer req)
    where
      path = Wai.pathInfo req


mainApp :: IORef ClientList -> Wai.Application
mainApp ref = websocketsOr WebSockets.defaultConnectionOptions (websocketsServerApp ref) normalServerApp

staticServer :: Wai.Application
staticServer = staticApp $ settings { ssIndices = indices }
  where
    settings = defaultWebAppSettings "../client"
    indices = fromJust $ toPieces ["index.html"]

main :: IO ()
main = do
  ref <- newIORef []
  Warp.run 8080 (router ref)

