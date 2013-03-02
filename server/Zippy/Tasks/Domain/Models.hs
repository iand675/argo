{-# LANGUAGE MultiParamTypeClasses #-}
module Zippy.Tasks.Domain.Models where
import Data.Time
import Data.Text (Text)
import Zippy.Base.Common
import Zippy.Base.Domain
import Zippy.Base.Model (Changeset(..))
import qualified Zippy.Tasks.Web.Models as Web
import Zippy.User.Domain.Models

data Tag = Tag

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

initializeList :: UTCTime -> Key User -> Web.NewList -> List
initializeList t u l = List
    { listName = Web.newListName l
    , listOwner = rekey u
    , listCreator = rekey u
    , listAdministrators = map rekey $ maybe [] id $ Web.newListAdministrators l
    , listGroup = fmap rekey $ Web.newListGroup l
    , listCreatedAt = t
    , listIcon = Web.newListIcon l
    }

data Group = Group
    { groupName    :: Text
    , groupOwner   :: Key User
    , groupMembers :: [Key User]
    , groupActive  :: Bool
    } deriving (Read, Show, Eq)

asGroup :: Group -> Web.Group
asGroup g = Web.Group
    { Web.groupName = groupName g
    , Web.groupOwner = rekey $ groupOwner g
    , Web.groupMembers = map rekey $ groupMembers g
    }

initializeGroup :: Key User -> Web.NewGroup -> Group
initializeGroup u g = Group
    { groupName = Web.newGroupName g
    , groupOwner = rekey u
    , groupMembers = map rekey $ maybe [] id $ Web.newGroupMembers g
    , groupActive = True
    }

instance Changeset Task Web.TaskChangeset where
    apply cs u = undefined

instance Changeset List Web.ListChangeset where
    apply cs u = undefined

instance Changeset Group Web.GroupChangeset where
    apply cs u = undefined