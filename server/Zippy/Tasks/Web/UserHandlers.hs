{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.UserHandlers where
import Control.Monad.Trans
import Data.Char
import qualified Data.Text as T
import Data.Time.Clock
import Network.HTTP.Types.Status
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Web
import Zippy.User.Domain.Models
import Zippy.User.Session
import Zippy.User.Web.Models
import qualified Zippy.Tasks.Domain.Models as M
import qualified Zippy.Tasks.Data.Group as S
import qualified Zippy.User.Data.Service as S

validateUser, validateEmail, validateUserUsername, validatePassword, validateUserName :: NewUser -> Bool
validateUser u = all ($ u) [validateEmail, validateUserUsername, validatePassword, validateUserName]

validateEmail u = (not . T.any isSpace $ newUserEmail u) && (not . T.null $ newUserEmail u)
validateUserUsername u = (not . T.any isSpace $ newUserUsername u) && (not . T.null $ newUserUsername u)
validateUserName = not . T.null . newUserName
validatePassword = not . T.null . newUserPassword

listUsers :: Handler c ()
listUsers = raise "listUsers: unimplemented"

createUser :: Handler c ()
createUser = do
	timestamp <- now
	user <- jsonData
	if not $ validateUser user
		then status badRequest400
		else withData (S.createUser $ initializeUser user timestamp) $ \r -> do
			status created201
			json $ asCurrentUser $ value r

getUser :: Handler c ()
getUser = do
	userId <- param "user"
	withData (S.getUser $ Key userId) (json . asUser . value)

getCurrentUser :: Handler c ()
getCurrentUser = do
	userId <- currentUserId
	withData (S.getUser userId) (json . asCurrentUser . value)

listUserGroups :: Handler c ()
listUserGroups = do
	userId <- param "user"
	withData (S.getUserGroups $ Key userId) (json . map (M.asGroup . value))

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
