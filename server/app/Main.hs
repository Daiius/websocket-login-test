{-# LANGUAGE OverloadedStrings #-}

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

immediatePutStrLn :: String -> IO ()
immediatePutStrLn text = do
  putStrLn text
  hFlush stdout

websocketsServerApp :: WebSockets.ServerApp
websocketsServerApp pendingConnection = do
  immediatePutStrLn "Websokets server responding!"
  connection <- WebSockets.acceptRequest pendingConnection
  hFlush stdout

normalServerApp :: Wai.Application
normalServerApp _ respond = do
  immediatePutStrLn "Normal server responding!"
  respond $ Wai.responseFile status200 [("Content-Type", "text/html")] "../client/index.html" Nothing

router :: Wai.Application
router req
  | null path = mainApp req
  | otherwise = trace (show path) (staticServer req)
    where
      path = Wai.pathInfo req

mainApp :: Wai.Application
mainApp = websocketsOr WebSockets.defaultConnectionOptions websocketsServerApp normalServerApp

staticServer :: Wai.Application
staticServer = staticApp $ settings { ssIndices = indices }
  where
    settings = defaultWebAppSettings "../client"
    indices = fromJust $ toPieces ["index.html"]

main :: IO ()
main = Warp.run 8080 router 