module Openspace (server, ServerState (..)) where

import Openspace.Types
import Snap
import Snap.Core
import Control.Monad.State
import qualified Network.SocketIO as SocketIO

import qualified Control.Concurrent.STM as STM

data ServerState = ServerState { appState :: STM.TVar AppState }

--server :: ServerState -> StateT SocketIO.RoutingTable Snap.Snap ()
server state = do
  liftIO $ putStrLn "Hi"
  SocketIO.on "message" $ \(a) ->
    SocketIO.broadcast "message" (a :: Action)