{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Client where
import Data.Aeson
import Data.Monoid
import Data.ByteString
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.Tasks.Web.Models
import Zippy.User.Web.Models

basicHandlers :: FromJSON a => [Handler a]
basicHandlers = [ bodyOn 200, nothingOn 404 ]

createdHandlers :: FromJSON a => [Handler a]
createdHandlers = [ bodyOn 201, nothingOn 404 ]

createTask :: NewTask -> ClientResult Task
createTask = post "/tasks" createdHandlers

getTask :: Key Task -> ClientResult Task
getTask key = get ("/tasks/" <> keyToBS key) basicHandlers

listUserTasks :: Key User -> ClientResult [Task]
listUserTasks = undefined

updateTask :: Key Task -> TaskChangeset -> ClientResult Task
updateTask key = post ("/tasks/" <> keyToBS key) basicHandlers

createUserList :: Key User -> NewList -> ClientResult List
createUserList key = post ("/users/" <> keyToBS key <> "/lists") createdHandlers

createGroupList :: Key Group -> NewList -> ClientResult List
createGroupList key = post ("/groups/" <> keyToBS key <> "/lists") createdHandlers

updateList :: Key List -> ListChangeset -> ClientResult List
updateList key = post ("/lists/" <> keyToBS key <> "/lists") basicHandlers

getList :: Key List -> ClientResult List
getList key = get ("/lists/" <> keyToBS key) basicHandlers

listUserLists :: Key User -> ClientResult [List]
listUserLists key = undefined

listGroupLists :: Key Group -> ClientResult [List]
listGroupLists key = undefined

createGroup :: NewGroup -> ClientResult Group
createGroup = post "/groups" createdHandlers

getGroup :: Key Group -> ClientResult Group
getGroup key = get ("/groups/" <> keyToBS key) basicHandlers

listUserGroups :: Key User -> ClientResult [List]
listUserGroups = undefined
