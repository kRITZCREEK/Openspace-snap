{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Openspace.Types where

import qualified Data.Map as M
import Data.Aeson
import Data.Text
import GHC.Generics

-----------------
-- | TopicType |--
-----------------

data TopicType = Discussion
               | Presentation
               | Workshop
               deriving (Show, Eq, Generic)

instance FromJSON TopicType
instance ToJSON TopicType

-- Used to fill Dropdowns etc.
-- TODO: Find a proper way to enumerate Union Datatypes
topicTypes :: [TopicType]
topicTypes = [Discussion, Presentation, Workshop]

--------------
-- | Topics |--
--------------

data Topic = Topic
  { topic :: Text
  , typ :: TopicType
  } deriving (Show, Eq, Generic)

instance FromJSON Topic
instance ToJSON Topic

------------
-- | Slot |--
------------

data Slot = Slot
  { room :: Room
  , block :: Block
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Slot
instance ToJSON Slot

------------
-- | Room |--
------------

data Room = Room
  { name :: Text
  , capacity :: Integer
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Room
instance ToJSON Room

-------------
-- | Block |--
-------------
data Timerange = Timerange
  { start :: Text
  , end :: Text
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Timerange
instance ToJSON Timerange

data Block = Block
  { description :: Text
  , range :: Timerange
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Block
instance ToJSON Block

---------------
-- | Actions |--
---------------

data Action = AddTopic Topic
            | DeleteTopic Topic
            | AddRoom Room
            | DeleteRoom Room
            | AddBlock Block
            | DeleteBlock Block
            | AssignTopic Slot Topic
            | UnassignTopic Topic
            | ShowError String
            | NOP
            deriving (Show, Eq, Generic)

instance FromJSON Action
instance ToJSON Action

-------------------------
-- | Entire AppState |--
-------------------------

--type Timeslot = (Slot, Topic)

data AppState = AppState { topics :: [Topic]
                         , rooms :: [Room]
                         , blocks :: [Block]
                         , timeslots :: M.Map Slot Topic
                         }
              deriving(Show, Eq, Generic)

--instance ToJSON AppState
--instance FromJSON AppState

 --------------------
 -- | Dummy Values |--
 --------------------
emptyState :: AppState
emptyState = AppState { topics = [], rooms = [], blocks = [], timeslots = M.empty }

myRoom = Room {name = "Berlin", capacity =  100}
myRoom1 = Room {name= "Hamburg", capacity= 80}
myRoom2 = Room {name= "Koeln", capacity= 30}

myBlock = Block { description="First", range= Timerange { start= "8=00am", end= "10=00am"} }
myBlock1 = Block { description="Second", range= Timerange { start= "10=00am", end= "12=00am"} }

mySlot = Slot {room=myRoom, block=myBlock}
mySlot1 = Slot {room=myRoom1, block=myBlock1}

myTopic = Topic {topic="Purescript is great", typ=Workshop}
myTopic1 = Topic {topic="Reactive Design", typ=Presentation}
myTopic2 = Topic {topic="Functional Javascript", typ=Discussion}
myTopic3 = Topic {topic="Enemy of the State", typ=Presentation}
myTopic4 = Topic {topic="Wayyyyyyy too long name for a Topic.", typ=Workshop}
myTopic5 = Topic {topic="fix", typ=Discussion}

myState1 = AppState { topics= [myTopic, myTopic1, myTopic2, myTopic3, myTopic4, myTopic5]
                    , rooms = [myRoom, myRoom1, myRoom2]
                    , blocks = [myBlock, myBlock1]
                    , timeslots= M.fromList [(mySlot, myTopic), (mySlot1, myTopic1)]
                    }
