{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.TaskHandlers where
import Network.HTTP.Types.Status
import Zippy.Base.Common
import Zippy.Base.Web
import Zippy.Accounts.Session
import qualified Zippy.Tasks.Data.Task as T
import Zippy.Tasks.Domain.Mappers
import Zippy.Tasks.Web.Types

createTask :: Handler c ()
createTask = authenticate (status unauthorized401) $ \userId -> do
	timestamp <- now
	newTask <- jsonData
	withData (T.createTask $ initializeTask timestamp userId newTask) $ \r -> do
		status created201
		json $ fmap (toModel task) r

listTasks :: Handler c ()
listTasks = raise "unimplemented"

getTask :: Handler c ()
getTask = do
	taskId <- param "task"
	withData (T.getTask $ Key taskId) (json . toModel task . value)

updateTask :: Handler c ()
updateTask = raise "unimplemented"

archiveTask :: Handler c ()
archiveTask = raise "unimplemented"
