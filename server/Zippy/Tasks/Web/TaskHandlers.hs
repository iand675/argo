{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.TaskHandlers where
import Network.HTTP.Types.Status
import Zippy.Base.Common
import Zippy.Base.Web
import Zippy.User.Session
import Zippy.Tasks.Domain.Models
import qualified Zippy.Tasks.Data.Task as T

createTask :: Handler c ()
createTask = do
	timestamp <- now
	userKey <- currentUserId
	task <- jsonData
	withData (T.createTask $ initializeTask timestamp userKey task) $ \r -> do
		status created201
		json $ fmap asTask r

listTasks :: Handler c ()
listTasks = raise "unimplemented"

getTask :: Handler c ()
getTask = raise "unimplemented"

updateTask :: Handler c ()
updateTask = raise "unimplemented"

archiveTask :: Handler c ()
archiveTask = raise "unimplemented"
