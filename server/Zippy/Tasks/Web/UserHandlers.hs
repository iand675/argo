{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.UserHandlers where
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Status
import Zippy.Base.Web
import qualified Zippy.User.Model.CurrentUser as C
import qualified Zippy.User.Model.User as U

listUsers :: Handler c ()
listUsers = raise "unimplemented"

createUser :: Handler c ()
createUser = do
	mUser <- fmap fromNewUser jsonData
	case mUser of
		Left issue -> text issue -- 400?
		Right user -> do
			mCreated <- service $ createUser user
			case mCreated of
				Left issue -> text issue -- should be 500 or 400, depending?
				Right newUser -> json $ toCurrentUserModel newUser

getUser :: Handler c ()
getUser = do
	userId <- param "user"
	mu <- service $ getUser $ key userId
	case mu of
		Nothing -> status notFound404
		Just u  -> json $ toUserModel u

getCurrentUser :: Handler c ()
getCurrentUser = do
	userId <- return "iand675"
	mu <- service $ getUser $ key userId
	case mu of
		Nothing -> status notFound404
		Just u  -> json $ toCurrentUserModel u

listUserGroups :: Handler c ()
listUserGroups = raise "unimplemented"

authorizeUser :: Handler c ()
authorizeUser = do
	signInDto <- jsonBody
	service $ 
