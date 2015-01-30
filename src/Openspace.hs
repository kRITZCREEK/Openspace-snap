{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Openspace (server, ServerState (..)) where

import Openspace.Types
import Openspace.Engine
import Foundation

import Snap.Snaplet.PostgresqlSimple

import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Applicative
--import Debug.Trace
import qualified Network.SocketIO as SocketIO

import qualified Control.Concurrent.STM as STM

data ServerState = ServerState { appState :: STM.TVar AppState }

readState :: STM.TVar AppState -> ReaderT SocketIO.Socket IO AppState
readState = liftIO . STM.atomically . STM.readTVar

server :: ServerState -> StateT SocketIO.RoutingTable AppHandler ()
server servstate = do
  SocketIO.on "message" $ \ a -> do
    liftIO $ STM.atomically $ do
      newState <- evalAction a <$> STM.readTVar (appState servstate)
      STM.writeTVar (appState servstate) newState
    SocketIO.broadcast "message" a
  SocketIO.on "state" $ do
    state <- liftIO $ STM.atomically $ STM.readTVar (appState servstate)
    SocketIO.emit "state" (generateActions state)
  SocketIO.on "commit" $ do
    state <- readState $ appState servstate
    --result <- execute "select * from table" mystate
    return state
