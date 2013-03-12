{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Domain.Mappers where
import Data.Time
import Zippy.Accounts.Domain.Types
import Zippy.Base.Common
import qualified Zippy.Tasks.Data.Types as Data
import Zippy.Tasks.Domain.Types
import qualified Zippy.Tasks.Web.Types as Web

instance DomainData List Data.List where

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

instance DataDomain Data.List List where
    fromData l = List
        { listName = Data.listName l
        , listOwner = rekey $ Data.listOwner l
        , listCreator = rekey $ Data.listCreator l
        , listAdministrators = map rekey $ Data.listAdministrators l
        , listGroup = fmap rekey $ Data.listGroup l
        , listCreatedAt = Data.listCreatedAt l
        , listIcon = Data.listIcon l
        , listTasks = map rekey $ Data.listTasks l
        }

--domainToData :: Task -> Task
--domainToData d = Task
--    { name = taskName d
--    , description = taskDescription d
--    , status = taskStatus d
--    , createdAt = taskCreatedAt d
--    , dueAt = taskDueAt d
--    , priority = taskPriority d
--    , tags = []
--    , archivedAt = taskArchivedAt d
--    , timeEstimate = taskTimeEstimate d
--    , creator = rekey $ taskCreator d
--    , assignedTo = fmap rekey $ taskAssignedTo d
--    }

--instance DomainData Task Data.Task where
--    toData d = Data.Task
--        { taskName = name d
--        , taskDescription = description d
--        , taskStatus = status d
--        , taskCreatedAt = createdAt d
--        , taskDueAt = dueAt d
--        , taskPriority = priority d
--        , taskTags = []
--        , taskArchivedAt = archivedAt d
--        , taskTimeEstimate = timeEstimate d
--        , taskCreator = rekey $ creator d
--        , taskAssignedTo = fmap rekey $ assignedTo d
--        }


instance ModelDomain Web.List List where

instance DomainModel List Web.List where
    toModel _ l = Web.List
        { Web.listName           = listName l
        , Web.listOwner          = rekey $ listOwner l
        , Web.listCreator        = rekey $ listCreator l
        , Web.listAdministrators = map rekey $ listAdministrators l
        , Web.listGroup          = fmap rekey $ listGroup l
        , Web.listCreatedAt      = listCreatedAt l
        , Web.listIcon           = listIcon l
        , Web.listTasks          = map rekey $ listTasks l
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
    , listTasks = []
    }

initializeTask :: UTCTime -> Key User -> Web.NewTask -> Task
initializeTask ts k t = Task
    { taskName = Web.newTaskName t
    , taskDescription = Web.newTaskDescription t
    , taskStatus = ""
    , taskCreatedAt = ts
    , taskDueAt = Nothing
    , taskPriority = Nothing
    , taskTags = []
    , taskArchivedAt = Nothing
    , taskTimeEstimate = Nothing
    , taskCreator = k
    , taskAssignedTo = Nothing
    }

instance DomainModel Task Web.Task where
    toModel _ t = Web.Task
        { Web.taskName         = taskName         t
        , Web.taskDescription  = taskDescription  t
        , Web.taskTags         = []
        , Web.taskCreator      = rekey $ taskCreator t
        , Web.taskAssignedTo   = fmap rekey $ taskAssignedTo t
        , Web.taskTasks        = []
        , Web.taskHistory      = []
        }
