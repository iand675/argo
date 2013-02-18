{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Client where
import Data.Monoid
import Data.ByteString
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.User.Web.Models

-- listUsers :: UserM [User]

createUser :: NewUser -> ClientResult (Maybe CurrentUser)
createUser = post "/users"
    [ bodyOn 200
    , nothingOn 404
    ]

getCurrentUser :: ClientResult (Maybe CurrentUser)
getCurrentUser = get "/users/me"
	[ bodyOn 200 
	, nothingOn 404
	]

getUser :: Key User -> ClientResult (Maybe User)
getUser key = get ("/users/" <> keyToBS key)
	[ bodyOn 200
	, nothingOn 404
	]

-- listUserGroups :: Text -> UserM [Group]
-- listUserGroups username = UserM $ get ("/v1/users/" <> username) 200
