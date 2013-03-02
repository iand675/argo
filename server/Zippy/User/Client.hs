{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Client where
import Data.Aeson
import Data.Monoid
import Data.ByteString
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.User.Web.Models
import Zippy.Tasks.Web.Models

-- listUsers :: UserM [User]
basicHandlers :: FromJSON a => [Handler (Maybe a)]
basicHandlers = [ bodyOn 200, nothingOn 404, nothingOn 400 ]

createdHandlers :: FromJSON a => [Handler (Maybe a)]
createdHandlers = [ bodyOn 201, nothingOn 400 ]

createUser :: NewUser -> ClientResult (Maybe CurrentUser)
createUser = post "/users" createdHandlers

getCurrentUser :: ClientResult (Maybe CurrentUser)
getCurrentUser = get "/users/me" basicHandlers

getUser :: Key User -> ClientResult (Maybe User)
getUser key = get ("/users/" <> htmlKey key) basicHandlers

listUserGroups :: Key User -> ClientResult (Maybe [Group])
listUserGroups key = get ("/users/" <> htmlKey key <> "/groups") basicHandlers
