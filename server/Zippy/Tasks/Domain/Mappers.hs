{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Domain.Mappers where
import Data.Time
import Zippy.Accounts.Domain.Types
import Zippy.Base.Common
import Zippy.Tasks.Domain.Types
import qualified Zippy.Tasks.Web.Types as Web

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

