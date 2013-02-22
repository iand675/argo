{-# LANGUAGE MultiParamTypeClasses #-}
module Zippy.Tasks.Domain.Models where
import Data.Time
import Data.Text (Text)
import Zippy.Base.Common
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
    , listOwner          :: Key User
    , listCreator        :: Key User
    , listAdministrators :: [Key User]
    , listGroup          :: Maybe (Key Group)
    , listCreatedAt      :: UTCTime
    , listIcon           :: Maybe Text
    }

asList :: List -> Web.List
asList l = Web.List
    { Web.listName           = listName l
    , Web.listOwner          = rekey $ listOwner l
    , Web.listCreator        = rekey $ listCreator l
    , Web.listAdministrators = map rekey $ listAdministrators l
    , Web.listGroup          = fmap rekey $ listGroup l
    , Web.listCreatedAt      = listCreatedAt l
    , Web.listIcon           = listIcon l
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