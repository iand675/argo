module Zippy.Tasks.Data.Group where

class GroupDestination a where {}
instance GroupDestination User where {}

createGroup :: Group -> MultiDb (Key Group)
createGroup = undefined

getGroup :: Key Group -> MultiDb (Maybe Group)
getGroup = undefined

getGroups :: GroupDestination a => Key a -> MultiDb [Group]
getGroups = undefined

updateGroup :: Key Group -> Changeset Group -> MultiDb Group
updateGroup = undefined

archiveGroup :: Key Group -> MultiDb ()
archiveGroup = undefined