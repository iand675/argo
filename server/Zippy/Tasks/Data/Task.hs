module Zippy.Tasks.Data.Task where
import Zippy.Base.Data

data Task = Task
	{ name :: Text
	, description :: Text
	, tags :: Text
	, creator :: Key User
	}

class TaskDestination a where {}
instance TaskDestination User where {}
instance TaskDestination List where {}
instance TaskDestination Group where {} 

createTask :: TaskDestination a => Key a -> Task -> MultiDb (Key Group)
createTask k t = undefined

getTask :: Key Task -> MultiDb (Maybe Task)
getTask = undefined

getTasks :: TaskDestination a => Key a -> MultiDb [Task]
getTasks = undefined

updateTask :: Key Task -> Changeset Task -> MultiDb Task
updateTask = undefined

archiveTask :: Key Task -> MultiDb ()
archiveTask = undefined