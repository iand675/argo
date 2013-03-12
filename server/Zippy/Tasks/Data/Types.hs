{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Data.Types where
import Data.Text (Text)
import Data.Time (UTCTime)
import Zippy.Accounts.Domain.Types (User, Group)
import Zippy.Base.Common
import Zippy.Base.JSON
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

instance DataDomain Task Domain.Task where

instance DomainData Domain.Task Task where

