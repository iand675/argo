{-# LANGUAGE OverloadedStrings #-}
module Zippy.Accounts.Web.UserHandlers where
import Control.Monad.Trans
import Data.Char
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Network.HTTP.Types.Status
import qualified Zippy.Accounts.Data.Service as S
import qualified Zippy.Accounts.Data.Group as S
import Zippy.Accounts.Domain.Mappers
import Zippy.Accounts.Session
import Zippy.Accounts.Web.Types
import Zippy.Base.Common
import Zippy.Base.Web

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
	webUser <- jsonData
	newUser <- liftIO $ initializeUser webUser timestamp
	if not $ validateUser webUser
		then status badRequest400
		else withData (S.createUser newUser) $ \r -> do
			status created201
			header "Set-Cookie" $ "ZippyAuth=" <> L.fromStrict (newUserUsername webUser) <> "; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT;"
			json $ toModel currentUser $ value r

getUser :: Handler c ()
getUser = do
	userId <- param "user"
	withData (S.getUser $ Key userId) (json . toModel user . value)

getCurrentUser :: Handler c ()
getCurrentUser = authenticate (status unauthorized401) $ \userId -> do
	withData (S.getUser userId) (json . toModel currentUser . value)

listUserGroups :: Handler c ()
listUserGroups = do
	userId <- param "user"
	withData (S.getUserGroups $ Key userId) (json . map (toModel group . value))

signIn :: Handler c ()
signIn = do
	signInReq <- jsonData
	withData (S.signIn (signInUsername signInReq) (signInPassword signInReq)) $ \mcookie ->
		case mcookie of
			Nothing -> status badRequest400
			Just _ -> do
				status noContent204
				header "Set-Cookie" $ "ZippyAuth=" <> L.fromStrict (signInUsername signInReq) <> "; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT;"
	--request <- jsonData
	--mu <- runData $ S.signIn (signInUsername request) (signInPassword request)
	--if mu
	--	then do
	--		session <- runData $ S.createSession $ Key $ signInUsername request
	--		setCookie "zippy-session" k session
	--	else status badRequest400
