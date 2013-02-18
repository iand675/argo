{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.Tasks.Data.Task where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe
import Data.Proxy
import Data.Time
import Zippy.Base.Common
import Zippy.Base.Model
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import Zippy.Tasks.Data.List
import Zippy.User.Data.User (User, creatorIx, assignedToIx)

task :: Proxy Task
task = Proxy

instance Namespace Task where
	namespace = const "task"

data Task = Task
    { name         :: Text
    , description  :: Maybe Text
    , status       :: Text
    , createdAt    :: UTCTime
    , dueAt        :: Maybe UTCTime
    , priority     :: Maybe Text
    , tags         :: [Text]
    , archivedAt   :: Maybe UTCTime
    , timeEstimate :: Maybe Text
    , creator      :: Key User
    , assignedTo   :: Key User
    , taskList     :: Key List
    --, taskHistory  :: [TaskChangeset]
    --, taskRecurrences  :: Maybe RecurrenceRule
    } deriving (Read, Show, Eq)

deriveJSON id ''Task

instance O.AsContent Task where
    indexes t = map ($ t) [listIx . taskList, creatorIx . creator, assignedToIx . assignedTo]
    fromContent = decode . C.value
    value = encode
    contentType = const O.jsonContent

createTask :: Task -> O.Riak b (Maybe (Key Task))
createTask t = do
	pr <- putNew task () t
	return $! fmap Key $ C.key pr

getTask :: Key Task -> O.Riak b (Maybe Task)
getTask k = do
	gr <- get task () k
	return $! (O.fromContent <=< justOne . C.getContent) gr

getListTasks :: Key List -> O.Riak b [{- TODO: Entity -} Task]
getListTasks k = do
    -- TODO: once linkwalking / mapreduce is better understood,
    -- consider replacing indexing with something like this:
    --gr <- linkWalk k [(namespace task, Nothing, True)] list
    keys <- indexEq task (namespace list) (fromKey k)
    values <- mapM (get task ()) keys
    -- TODO: do something better than filtering out invalid values
    return $! mapMaybe (O.fromContent <=< justOne . C.getContent) values
