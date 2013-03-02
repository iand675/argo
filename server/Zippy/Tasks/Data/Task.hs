{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.Tasks.Data.Task where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe
import Data.Proxy
import Data.Time
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Model
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import qualified Zippy.Tasks.Domain.Models as Domain
import Zippy.User.Data.User (User, creatorIx, assignedToIx)

domainToData :: Domain.Task -> Task
domainToData d = Task
    { name = Domain.taskName d
    , description = Domain.taskDescription d
    , status = Domain.taskStatus d
    , createdAt = Domain.taskCreatedAt d
    , dueAt = Domain.taskDueAt d
    , priority = Domain.taskPriority d
    , tags = []
    , archivedAt = Domain.taskArchivedAt d
    , timeEstimate = Domain.taskTimeEstimate d
    , creator = rekey $ Domain.taskCreator d
    , assignedTo = fmap rekey $ Domain.taskAssignedTo d
    }

instance DataRep Domain.Task Task where
    fromData d = Domain.Task
        { Domain.taskName = name d
        , Domain.taskDescription = description d
        , Domain.taskStatus = status d
        , Domain.taskCreatedAt = createdAt d
        , Domain.taskDueAt = dueAt d
        , Domain.taskPriority = priority d
        , Domain.taskTags = []
        , Domain.taskArchivedAt = archivedAt d
        , Domain.taskTimeEstimate = timeEstimate d
        , Domain.taskCreator = rekey $ creator d
        , Domain.taskAssignedTo = fmap rekey $ assignedTo d
        }

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
    , assignedTo   :: Maybe (Key User)
    --, taskList     :: Key Domain.List
    --, taskHistory  :: [TaskChangeset]
    --, taskRecurrences  :: Maybe RecurrenceRule
    } deriving (Read, Show, Eq)

deriveJSON id ''Task

instance O.AsContent Task where
    fromContent = decode . C.value
    value = encode
    contentType = const O.jsonContent

createTask :: Domain.Task -> MultiDb (Entity Domain.Task)
createTask t = riak $ do
	pr <- putNew task () $ domainToData t
	return $! fmap ((flip Entity $ t) . Key) $ ifNothing AlreadyExists $ C.key pr

getTask :: Key Domain.Task -> MultiDb (Entity Domain.Task)
getTask k = riak $ do
	gr <- get task () $ rekey k
	return $! fmap result $ (ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) gr
    where result = Entity k . fromData

getListTasks :: Key Domain.List -> MultiDb [Entity Domain.Task]
getListTasks k = undefined 
{- riak $ do
    -- TODO: once linkwalking / mapreduce is better understood,
    -- consider replacing indexing with something like this:
    --gr <- linkWalk k [(namespace task, Nothing, True)] list
    keys <- indexEq task (namespace list) (fromKey k)
    values <- mapM (get task ()) keys
    -- TODO: do something better than filtering out invalid values
    return $! mapMaybe (O.fromContent <=< justOne . C.getContent) values
    -}
