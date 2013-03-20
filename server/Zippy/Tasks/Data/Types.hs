{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Data.Types where
import Data.Time (UTCTime)
import Zippy.Accounts.Domain.Types (User, Group)
import Zippy.Base.Common
import Zippy.Base.JSON
import qualified Zippy.Riak.Types as Riak
import qualified Zippy.Tasks.Domain.Types as Domain

data List = List
    { listName :: Text
    , listOwner :: Key User
    , listCreator :: Key User
    , listAdministrators :: [Key User]
    , listGroup :: Maybe (Key Group)
    , listCreatedAt :: UTCTime
    , listIcon :: Maybe Text
    , listTasks :: [Key Task]
    } deriving (Read, Show, Eq)

data Task = Task
    { taskName         :: Text
    , taskDescription  :: Maybe Text
    , taskStatus       :: Text
    , taskCreatedAt    :: UTCTime
    , taskDueAt        :: Maybe UTCTime
    , taskPriority     :: Maybe Text
    , taskTags         :: [Text]
    , taskArchivedAt   :: Maybe UTCTime
    , taskTimeEstimate :: Maybe Text
    , taskCreator      :: Key User
    , taskAssignedTo   :: Maybe (Key User)
    --, taskList     :: Key Domain.List
    --, taskHistory  :: [TaskChangeset]
    --, taskRecurrences  :: Maybe RecurrenceRule
    } deriving (Read, Show, Eq)

deriveJSON (stripPrefix 0) ''List
deriveJSON (stripPrefix 0) ''Task

instance Namespace List where
    namespace = const "list"

instance Namespace Task where
    namespace = const "task"

instance Riak.Namespace List where
    namespace = const "list"

instance Riak.Namespace Task where
    namespace = const "task"

instance DataDomain Task Domain.Task where
    fromData t = Domain.Task
        { Domain.taskName         = taskName         t
        , Domain.taskDescription  = taskDescription  t
        , Domain.taskStatus       = taskStatus       t
        , Domain.taskCreatedAt    = taskCreatedAt    t
        , Domain.taskDueAt        = taskDueAt        t
        , Domain.taskPriority     = taskPriority     t
        , Domain.taskTags         = []
        , Domain.taskArchivedAt   = taskArchivedAt   t
        , Domain.taskTimeEstimate = taskTimeEstimate t
        , Domain.taskCreator      = rekey $ taskCreator t
        , Domain.taskAssignedTo   = fmap rekey $ taskAssignedTo t
        }

instance DomainData Domain.Task Task where
    toData t = Task
        { taskName         = Domain.taskName         t
        , taskDescription  = Domain.taskDescription  t
        , taskStatus       = Domain.taskStatus       t
        , taskCreatedAt    = Domain.taskCreatedAt    t
        , taskDueAt        = Domain.taskDueAt        t
        , taskPriority     = Domain.taskPriority     t
        , taskTags         = []
        , taskArchivedAt   = Domain.taskArchivedAt   t
        , taskTimeEstimate = Domain.taskTimeEstimate t
        , taskCreator      = rekey $ Domain.taskCreator t
        , taskAssignedTo   = fmap rekey $ Domain.taskAssignedTo t
        }

instance DomainData Domain.List List where
    toData l = List
        { listName = Domain.listName l
        , listOwner = rekey $ Domain.listOwner l
        , listCreator = rekey $ Domain.listCreator l
        , listAdministrators = map rekey $ Domain.listAdministrators l
        , listGroup = fmap rekey $ Domain.listGroup l
        , listCreatedAt = Domain.listCreatedAt l
        , listIcon = Domain.listIcon l
        , listTasks = map rekey $ Domain.listTasks l
        }
--domainToData :: List -> List
--domainToData l = List
--    { listName = listName l
--    , listOwner = rekey $ listOwner l
--    , listCreator = rekey $ listCreator l
--    , listAdministrators = map rekey $ listAdministrators l
--    , listGroup = fmap rekey $ listGroup l
--    , listCreatedAt = listCreatedAt l
--    , listIcon = listIcon l
--    , listTasks = listTasks l
--    }

instance DataDomain List Domain.List where
    fromData l = Domain.List
        { Domain.listName = listName l
        , Domain.listOwner = rekey $ listOwner l
        , Domain.listCreator = rekey $ listCreator l
        , Domain.listAdministrators = map rekey $ listAdministrators l
        , Domain.listGroup = fmap rekey $ listGroup l
        , Domain.listCreatedAt = listCreatedAt l
        , Domain.listIcon = listIcon l
        , Domain.listTasks = map rekey $ listTasks l
        }
