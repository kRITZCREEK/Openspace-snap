{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Openspace (server, ServerState (..)) where

import Openspace.Types
import Openspace.Engine
import Control.Monad.State
import Control.Applicative
import Debug.Trace
import qualified Network.SocketIO as SocketIO

import qualified Control.Concurrent.STM as STM

data ServerState = ServerState { appState :: STM.TVar AppState }

--server :: ServerState -> StateT SocketIO.RoutingTable Snap.Snap ()
server servstate = do
  SocketIO.on "message" $ \ a -> do
    liftIO $ STM.atomically $ do
      newState <- evalAction a <$> STM.readTVar (appState servstate)
      STM.writeTVar (appState servstate) newState
    SocketIO.broadcast "message" a
  SocketIO.on "state" $ do
    mystate <- liftIO $ STM.atomically $ STM.readTVar (appState servstate)
    SocketIO.emit "state" (generateActions mystate)
