{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Web.Types where
import Data.Proxy
import Data.Time
import Zippy.Base.Common
import Zippy.Base.JSON
import Zippy.Accounts.Web.Types

list :: Proxy List
list = Proxy

task :: Proxy Task
task = Proxy

type Icon = Text

data Sideload a = Reference { sideloadId :: Key a }
                | Loaded { sideloadValue :: Entity a }
                deriving (Read, Show, Eq)

data NewTask = NewTask
    { newTaskName        :: Text
    , newTaskList        :: Maybe (Key List)
    , newTaskDescription :: Maybe Text
    , newTaskAssignedTo  :: Maybe (Key User) -- Username
    } deriving (Read, Show, Eq)

data TaskChangeset = TaskChangeset
    { taskChangesetName        :: Maybe Text
    , taskChangesetList        :: Maybe (Key List)
    , taskChangesetDescription :: Maybe Text
    , taskChangesetAssignedTo  :: Maybe (Maybe (Key User))
    } deriving (Read, Show, Eq)

data Task = Task
    { taskName        :: Text
    , taskDescription :: Maybe Text
    , taskTags        :: [Text]
    , taskCreator     :: Key User
    , taskTasks       :: [Sideload Task]
    , taskAssignedTo  :: Maybe (Key User)
    , taskHistory     :: [()]
    } deriving (Read, Show, Eq)

data NewList = NewList
    { newListName           :: Text
    , newListAdministrators :: Maybe [Key User]
    , newListGroup          :: Maybe (Key Group)
    , newListIcon           :: Maybe Text
    } deriving (Read, Show, Eq)

data ListChangeset = ListChangeset
    { listChangesetName           :: Maybe Text
    , listChangesetAdministrators :: Maybe [Key User]  -- ? should this be Maybe (Maybe [Key User])
    , listChangesetGroup          :: Maybe (Key Group) -- ? ^likewise
    , listChangesetIcon           :: Maybe Icon
    } deriving (Read, Show, Eq)

data List = List
    { listName           :: Text
    , listOwner          :: Key User
    , listCreator        :: Key User
    , listAdministrators :: [Key User]
    , listGroup          :: Maybe (Key Group)
    , listCreatedAt      :: UTCTime
    , listIcon           :: Maybe Text
    , listTasks          :: [Key Task]
    } deriving (Read, Show, Eq)

jsonize 0 ''Sideload

jsonize 1 ''NewTask
jsonize 1 ''TaskChangeset
jsonize 0 ''Task

jsonize 1 ''NewList
jsonize 1 ''ListChangeset
jsonize 0 ''List