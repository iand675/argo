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

createUser :: NewUser -> ClientResult (Maybe CurrentUser)
createUser = post "/users" basicHandlers

getCurrentUser :: ClientResult (Maybe CurrentUser)
getCurrentUser = get "/users/me" basicHandlers

getUser :: Key User -> ClientResult (Maybe User)
getUser key = get ("/users/" <> keyToBS key) basicHandlers

listUserGroups :: Key User -> ClientResult (Maybe [Group])
listUserGroups key = get ("/users/" <> keyToBS key <> "/groups") basicHandlers
