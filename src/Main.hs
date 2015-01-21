{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Openspace (server, ServerState (..))
import Control.Applicative

import qualified Network.EngineIO.Snap as EIOSnap
import qualified Control.Concurrent.STM as STM
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Env
import qualified Snap.Http.Server.Config as Snap
import qualified Network.SocketIO as SocketIO

import Openspace.Types

--import Paths_chat (getDataDir)

main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO myState1
  socketIoHandler <- SocketIO.initialize EIOSnap.snapAPI (server state)
  --dataDir <- getDataDir
  Env.httpServe Snap.emptyConfig  $
    Snap.route [ ("/socket.io", socketIoHandler)
               , ("/", Snap.serveDirectory "static")
               ]