{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.UserHandlers where
import Control.Monad.Trans
import Data.Time.Clock
import Network.HTTP.Types.Status
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Web
import Zippy.User.Domain.Models
import qualified Zippy.User.Data.Service as S

listUsers :: Handler c ()
listUsers = raise "unimplemented"

createUser :: Handler c ()
createUser = do
	timestamp <- liftIO getCurrentTime
	user <- jsonData
	mCreated <- runData $ S.createUser $ initializeUser user timestamp
	case mCreated of
		Left issue -> handleDataError issue
		Right u -> json $ asCurrentUser $ value u

getUser :: Handler c ()
getUser = do
	userId <- param "user"
	mu <- runData $ S.getUser $ Key userId
	case mu of
		Left issue -> handleDataError issue
		Right u -> json $ asUser $ value u

getCurrentUser :: Handler c ()
getCurrentUser = do
	userId <- return "iand675"
	mu <- runData $ S.getUser $ Key userId
	case mu of
		Left issue -> handleDataError issue
		Right u -> json $ asCurrentUser $ value u

listUserGroups :: Handler c ()
listUserGroups = raise "unimplemented"

signIn :: Handler c ()
signIn = raise "unimplemented"

--authorizeUser :: Handler c ()
--authorizeUser = do
--	signInDto <- jsonBody
--	service $ 
