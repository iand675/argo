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
getTask key = get ("/tasks/" <> htmlKey key) basicHandlers

listUserTasks :: Key User -> ClientResult [Task]
listUserTasks = undefined

updateTask :: Key Task -> TaskChangeset -> ClientResult Task
updateTask key = post ("/tasks/" <> htmlKey key) basicHandlers

createUserList :: Key User -> NewList -> ClientResult List
createUserList key = post ("/users/" <> htmlKey key <> "/lists") createdHandlers

createGroupList :: Key Group -> NewList -> ClientResult List
createGroupList key = post ("/groups/" <> htmlKey key <> "/lists") createdHandlers

updateList :: Key List -> ListChangeset -> ClientResult List
updateList key = post ("/lists/" <> htmlKey key) basicHandlers

getList :: Key List -> ClientResult List
getList key = get ("/lists/" <> htmlKey key) basicHandlers

listUserLists :: Key User -> ClientResult [List]
listUserLists key = undefined

listGroupLists :: Key Group -> ClientResult [List]
listGroupLists key = undefined

createGroup :: NewGroup -> ClientResult Group
createGroup = post "/groups" createdHandlers

getGroup :: Key Group -> ClientResult Group
getGroup key = get ("/groups/" <> htmlKey key) basicHandlers

listUserGroups :: Key User -> ClientResult [List]
listUserGroups = undefined
