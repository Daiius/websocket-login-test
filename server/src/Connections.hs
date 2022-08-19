module Connections where

import qualified Network.WebSockets as WebSockets
import Data.Text (Text)


type ClientId   = Text
type ClientData = (ClientId, WebSockets.Connection)
type ClientList = [ClientData]


isClient :: ClientList -> ClientId -> Bool
isClient cl ci = any (\cd -> fst cd == ci) cl


addClient :: ClientList -> ClientData -> ClientList
addClient cl cd =
  if isClient cl $ fst cd then
    cl
  else
    cd : cl

removeClient :: ClientList -> ClientId -> ClientList
removeClient cl idToRemove = filter (\(i, _) -> i /= idToRemove) cl

