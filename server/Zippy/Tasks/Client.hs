{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Client where
import Data.Aeson
import Data.Monoid
import Data.ByteString
import Zippy.Accounts.Web.Types
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.Tasks.Web.Types

basicHandlers :: FromJSON a => [Handler (Maybe a)]
basicHandlers = [ bodyOn 200, nothingOn 401, nothingOn 404 ]

createdHandlers :: FromJSON a => [Handler (Maybe a)]
createdHandlers = [ bodyOn 201, nothingOn 401, nothingOn 404 ]

createTask :: NewTask -> ClientResult (Maybe (Entity Task))
createTask = post "/tasks" createdHandlers

getTask :: Key Task -> ClientResult (Maybe Task)
getTask key = get ("/tasks/" <> htmlKey key) basicHandlers

--listUserTasks :: Key User -> ClientResult [Task]
--listUserTasks = get ("/")

updateTask :: Key Task -> TaskChangeset -> ClientResult (Maybe (Entity Task))
updateTask key = post ("/tasks/" <> htmlKey key) basicHandlers

createList :: NewList -> ClientResult (Maybe (Entity List))
createList = post ("/lists") createdHandlers

updateList :: Key List -> ListChangeset -> ClientResult (Maybe (Entity List))
updateList key = post ("/lists/" <> htmlKey key) basicHandlers

getList :: Key List -> ClientResult (Maybe (Entity List))
getList key = get ("/lists/" <> htmlKey key) basicHandlers

listUserLists :: Key User -> ClientResult (Maybe [Entity List])
listUserLists key = get ("/users/" <> htmlKey key <> "/lists") basicHandlers

listGroupLists :: Key Group -> ClientResult (Maybe [Entity List])
listGroupLists key = undefined

createGroup :: NewGroup -> ClientResult (Maybe (Entity Group))
createGroup = post "/groups" createdHandlers

getGroup :: Key Group -> ClientResult (Maybe (Entity Group))
getGroup key = get ("/groups/" <> htmlKey key) basicHandlers

listUserGroups :: Key User -> ClientResult (Maybe [Entity Group])
listUserGroups key = get ("/users/" <> htmlKey key <> "/groups") basicHandlers
