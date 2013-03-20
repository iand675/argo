{-# LANGUAGE OverloadedStrings #-}
module Zippy.Accounts.Client where
import Data.Aeson
import Data.Monoid
import Data.ByteString
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.Accounts.Web.Types
--import Zippy.Tasks.Web.Types

-- listUsers :: UserM [User]
basicHandlers :: FromJSON a => [Handler (Maybe a)]
basicHandlers = [ bodyOn 200, nothingOn 400, nothingOn 401, nothingOn 404 ]

createdHandlers :: FromJSON a => [Handler (Maybe a)]
createdHandlers = [ bodyOn 201, nothingOn 400 ]

noContentHandlers :: [Handler Bool]
noContentHandlers = [(204, const $ Just True), (400, const $ Just False)]

createUser :: NewUser -> ClientResult (Maybe CurrentUser)
createUser = post "/users" createdHandlers

getCurrentUser :: ClientResult (Maybe CurrentUser)
getCurrentUser = get "/users/me" basicHandlers

getUser :: Key User -> ClientResult (Maybe User)
getUser key = get ("/users/" <> htmlKey key) basicHandlers

listUserGroups :: Key User -> ClientResult (Maybe [Group])
listUserGroups key = get ("/users/" <> htmlKey key <> "/groups") basicHandlers

signIn :: SignInRequest -> ClientResult Bool
signIn = post "/signin" noContentHandlers