{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.TaskHandlers where
import Network.HTTP.Types.Status
import Zippy.Base.Common
import Zippy.Base.Web
import Zippy.User.Session
import Zippy.Tasks.Domain.Models
import qualified Zippy.Tasks.Data.Task as T

createTask :: Handler c ()
createTask = authenticate (status unauthorized401) $ \userId -> do
	timestamp <- now
	task <- jsonData
	withData (T.createTask $ initializeTask timestamp userId task) $ \r -> do
		status created201
		json $ fmap asTask r

listTasks :: Handler c ()
listTasks = raise "unimplemented"

getTask :: Handler c ()
getTask = do
	taskId <- param "task"
	withData (T.getTask $ Key taskId) (json . asTask . value)

updateTask :: Handler c ()
updateTask = raise "unimplemented"

archiveTask :: Handler c ()
archiveTask = raise "unimplemented"
