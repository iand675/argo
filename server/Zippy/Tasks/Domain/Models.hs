{-# LANGUAGE MultiParamTypeClasses #-}
module Zippy.Tasks.Domain.Models where
import Data.Time
import Data.Text (Text)
import Zippy.Base.Domain
import Zippy.Base.Model (Changeset(..))
import qualified Zippy.Tasks.Web.Models as Web
import Zippy.User.Domain.Models

data Tag

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
    , taskAssignedTo   :: Key User
    --, taskRecurrences  :: Maybe RecurrenceRule
    }

data ListType = Plain
              | Smart

data List = List
    { listName           :: Text
    , listType           :: ListType
    , listOwner          :: Key User
    , listAdministrators :: [Key User]
    , listGroup          :: Maybe (Key Group)
    , listCreatedAt      :: UTCTime
    , listIcon           :: Text
    }

data Group = Group
    { groupName    :: Text
    , groupOwner   :: Key User
    , groupMembers :: [Key User]
    }

instance Changeset Task Web.TaskChangeset where
    apply cs u = undefined

instance Changeset List Web.ListChangeset where
    apply cs u = undefined

instance Changeset Group Web.GroupChangeset where
    apply cs u = undefined