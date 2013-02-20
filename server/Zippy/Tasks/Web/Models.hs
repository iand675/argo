{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Web.Models where
import Data.Time
import Zippy.Base.Common
import Zippy.Base.Model
import Zippy.User.Web.Models

type Icon = Text

data Sideload a = Reference { sideloadId :: Text }
                | Loaded { sideloadValue :: a }

data NewTask = NewTask
    { newTaskName        :: Text
    , newTaskDescription :: Text
    , newTaskAssignedTo  :: Key User -- Username
    }

data TaskChangeset = TaskChangeset
    { taskChangesetName        :: Maybe Text
    , taskChangesetDescription :: Maybe Text
    , taskChangesetAssignedTo  :: Maybe (Key User)
    }

data Task = Task
    { taskId          :: Key Task
    , taskName        :: Text
    , taskDescription :: Text
    , taskTags        :: [Text]
    , taskCreator     :: User
    , taskTasks       :: [Sideload Task]
    , taskAssignedTo  :: User
    , taskHistory     :: Sideload [()]
    }

data NewList = NewList
    { newListName           :: Text
    , newListAdministrators :: Maybe [Key User]
    , newListGroup          :: Maybe (Key Group)
    , newListIcon           :: Maybe Text
    }

data ListChangeset = ListChangeset
    { listChangesetName           :: Maybe Text
    , listChangesetAdministrators :: Maybe [Key User]  -- ? should this be Maybe (Maybe [Key User])
    , listChangesetGroup          :: Maybe (Key Group) -- ? ^likewise
    , listChangesetIcon           :: Maybe Icon
    }

data List = List
    { listId             :: Key List
    , listName           :: Text
    , listCreator        :: User
    , listAdministrators :: [User]
    , listGroup          :: Group
    , listCreatedDate    :: UTCTime
    , listModifiedDate   :: UTCTime
    , listIcon           :: Text
    , listTasks          :: [Task]
    }

data NewGroup = NewGroup
    { newGroupName    :: Text 
    , newGroupMembers :: Maybe [Key User]
    }

data GroupChangeset = GroupChangeset
    { groupChangesetName  :: Maybe Text
    , groupChangesetOwner :: Maybe (Key User)
    }

data Group = Group
    { groupId      :: Key Group
    , groupName    :: Text
    , groupOwner   :: User
    , groupMembers :: [Sideload User]
    --, groupLists   :: [Sideload List]
    }

jsonize 0 ''Sideload

jsonize 1 ''NewTask
jsonize 1 ''TaskChangeset
jsonize 0 ''Task

jsonize 1 ''NewGroup
jsonize 1 ''GroupChangeset
jsonize 0 ''Group

jsonize 1 ''NewList
jsonize 1 ''ListChangeset
jsonize 0 ''List