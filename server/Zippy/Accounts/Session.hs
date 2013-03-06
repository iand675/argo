{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Session where
import Zippy.Base.Common
import Zippy.Base.Web
import Zippy.User.Domain.Models

currentUserId :: Handler c (Maybe (Key User))
currentUserId = fmap (fmap toKey . lookup "ZippyAuth") cookies

authenticate :: Handler c () -> (Key User -> Handler c ()) -> Handler c ()
authenticate noAuth auth = do
	mUserKey <- currentUserId
	case mUserKey of
		Nothing -> noAuth
		Just k -> auth k