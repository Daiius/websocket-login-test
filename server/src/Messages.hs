{-# LANGUAGE OverloadedStrings #-}

module Messages where

import Data.Text (Text, split, unpack)
import Data.IORef (atomicModifyIORef, IORef, atomicWriteIORef)
import Network.WebSockets (Connection)

import Utils (immediatePutStrLn, immediatePrint)
import Connections 



handleMessage :: IORef ClientList -> IORef ClientId -> Connection -> Text -> IO ()
handleMessage ref identity connection message = do
  let tokens = split (== ',') message
  case head tokens of
    "name" -> handleNewConnection ref identity connection $ last tokens
    _      -> immediatePutStrLn $ "Unknown message: " ++ unpack message

handleNewConnection :: IORef ClientList -> IORef ClientId -> Connection -> Text -> IO ()
handleNewConnection ref identity connection name = do
  atomicWriteIORef identity name
  atomicModifyIORef ref addClientHelper
  immediatePutStrLn $ "Added client " ++ unpack name
  where
    addClientHelper list = (addClient list (name,connection), ())

