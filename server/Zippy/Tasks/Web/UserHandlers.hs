{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.UserHandlers where
import Control.Monad.Trans
import Data.Time.Clock
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Web
import Zippy.User.Domain.Models
import Zippy.User.Session
import Zippy.User.Web.Models
import qualified Zippy.User.Data.Service as S

listUsers :: Handler c ()
listUsers = raise "unimplemented"

createUser :: Handler c ()
createUser = do
	timestamp <- now
	user <- jsonData
	withData (S.createUser $ initializeUser user timestamp) (json . asCurrentUser . value)

getUser :: Handler c ()
getUser = do
	userId <- param "user"
	withData (S.getUser $ Key userId) (json . asUser . value)

getCurrentUser :: Handler c ()
getCurrentUser = do
	userId <- currentUserId
	withData (S.getUser userId) (json . asCurrentUser . value)

listUserGroups :: Handler c ()
listUserGroups = raise "unimplemented"

signIn :: Handler c ()
signIn = do
	return ()
	--request <- jsonData
	--mu <- runData $ S.signIn (signInUsername request) (signInPassword request)
	--if mu
	--	then do
	--		session <- runData $ S.createSession $ Key $ signInUsername request
	--		setCookie "zippy-session" k session
	--	else status badRequest400
