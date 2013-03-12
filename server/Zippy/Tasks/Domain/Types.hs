module Zippy.Tasks.Domain.Types where
import Data.Time
import Data.Text (Text)
import Zippy.Accounts.Domain.Types (User, Group)
import Zippy.Base.Common

data Tag = Tag
    deriving (Read, Show, Eq)

data Task = Task
    { taskName         :: Text
    , taskDescription  :: Maybe Text
    , taskStatus       :: Text
    , taskCreatedAt    :: UTCTime
    , taskDueAt        :: Maybe UTCTime
    , taskPriority     :: Maybe Text
    , taskTags         :: [Tag]
    , taskArchivedAt   :: Maybe UTCTime
    , taskTimeEstimate :: Maybe Text
    , taskCreator      :: Key User
    , taskAssignedTo   :: Maybe (Key User)
    --, taskRecurrences  :: Maybe RecurrenceRule
    } deriving (Read, Show, Eq)

data ListType = Plain
              | Smart

data List = List
    { listName           :: Text
    , listOwner          :: Key User
    , listCreator        :: Key User
    , listAdministrators :: [Key User]
    , listGroup          :: Maybe (Key Group)
    , listCreatedAt      :: UTCTime
    , listIcon           :: Maybe Text
    , listTasks          :: [Key Task]
    }
