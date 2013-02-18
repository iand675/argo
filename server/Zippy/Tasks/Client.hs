{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Client where
import Data.Monoid
import Data.ByteString
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.Tasks.Web.Models

basicHandlers :: FromJSON a => [Handler a]
basicHandlers = [ bodyOn 200, nothingOn 404 ]

createTask :: NewTask -> ClientResult (Maybe Task)
createTask = post "/tasks"
    [ bodyOn 201
    , nothingOn 404
    ]

getTask :: Key Task -> ClientResult (Maybe Task)
getTask key = get ("/tasks/" <> keyToBS key) basicHandlers

listUserTasks :: Key User -> ClientResult [Task]
listUserTasks = undefined

updateTask :: Key Task -> TaskChangeset -> ClientResult (Maybe Task)
updateTask key = post ("/tasks/" <> keyToBS key) basicHandlers