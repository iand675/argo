{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Client where
import Data.Monoid
import Data.ByteString
import Zippy.Base.Client
import Zippy.User.Model.CurrentUser
import Zippy.User.Model.User

-- listUsers :: UserM [User]

-- createUser :: NewUser -> UserM (Maybe Self)

getCurrentUser :: ClientResult (Maybe CurrentUser)
getCurrentUser = get "/v1/users/me"
	[ bodyOn 200 
	, nothingOn 404
	]

getUser :: ByteString -> ClientResult (Maybe User)
getUser username = get ("/v1/users/" <> username)
	[ bodyOn 200
	, nothingOn 404
	]

-- listUserGroups :: Text -> UserM [Group]
-- listUserGroups username = UserM $ get ("/v1/users/" <> username) 200
