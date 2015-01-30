{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation where

import Snap(get)
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple


data Openspace = Openspace
  { _db   :: Snaplet Postgres }

makeLenses ''Openspace

instance HasPostgres (Handler b Openspace) where
  getPostgresState = with db get

type AppHandler = Handler Openspace Openspace

-------------------------------------------------------------------------------
