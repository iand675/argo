module Zippy.Tasks.Data.List where
import Zippy.Base.Data

class ListDestination a where {}
instance ListDestination User where {}
instance ListDestination Group where {}

createList :: ListDestination a => Key a -> List -> MultiDb (Key List)
createList = undefined

getList :: Key List -> MultiDb (Maybe List)
getList = undefined

getLists :: ListDestination a => Key a -> MultiDb [List]
getLists = undefined

updateList :: Key List -> Changeset List -> MultiDb List
updateList = undefined

archiveList :: Key List -> MultiDb ()
archiveList = undefined