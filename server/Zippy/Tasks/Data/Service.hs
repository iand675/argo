module Zippy.Tasks.Data.Service where
import Data.Text (Text)
import Zippy.Base.Data
import qualified Zippy.Tasks.Domain.Models as Domain

type Username = Text
type Password = Text

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

createTask :: TaskDestination a => Key a -> Domain.Task -> MultiDb (Key Domain.Group)
createTask k t = do

getTask :: Key Domain.Task -> MultiDb (Maybe Domain.Task)
getTask = undefined

getTasks :: TaskDestination a => Key a -> MultiDb [Domain.Task]
getTasks = undefined

updateTask :: Key Domain.Task -> Domain.Task -> MultiDb Domain.Task
updateTask = undefined

createList :: TaskDestination a => Key a -> Domain.List -> MultiDb (Key Domain.List)

getList :: Key Domain.List -> MultiDb (Maybe Domain.List)

getLists :: TaskDestination a => Key a -> MultiDb [Domain.List]