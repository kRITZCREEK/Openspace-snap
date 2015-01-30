{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Openspace (server, ServerState (..))
import Control.Applicative

import Network.EngineIO.Snap
import Network.SocketIO
import qualified Control.Concurrent.STM as STM
import Snap
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Http.Server.Config as Snap

import Openspace.Types
import Foundation
--import Paths_openspace (getDataDir)

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

appInit :: AppHandler () -> SnapletInit Openspace Openspace
appInit socketIoHandler = makeSnaplet "openspace" "Backend Server for the OpenSpace App." Nothing $ do
  d <- nestSnaplet "db" db pgsInit
  addRoutes [ ("/socket.io", socketIoHandler)
            , ("/", Snap.serveDirectory "static") ]
  return $ Openspace d

main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO myState1
  socketIoHandler <- initialize snapAPI (server state)
  (_, app, _) <- runSnaplet Nothing (appInit socketIoHandler)
  Snap.httpServe Snap.defaultConfig app
