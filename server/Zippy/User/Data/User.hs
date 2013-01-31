{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.User.Data.User where
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time
import Database.Redis hiding (decode)
import Zippy.Base.Data
import Zippy.Base.Model

userKeys = Proxy :: Proxy User

instance Namespace User where
	namespace = const "user:"

data User = User
	{ name             :: Text
	, username         :: Text
	, avatar           :: Text
	, email            :: Text
	, passwordHash     :: Text
	, mailable         :: Bool
	, stripeCustomerId :: Text
	, created          :: UTCTime
	} deriving (Read, Show, Eq)

deriveJSON id ''User

createUser :: User -> MultiDb (Either Text (Key User))
createUser u = do
	--metric "user:create"
	let userBs = encodeUtf8 $ username u
	userCreated <- redis $ setnx (namespace userKeys <> userBs) $ toStrict $ encode u
	return $! case userCreated of
		Left r      -> Left $ "Redis error: " <> (pack $ show r)
		Right False -> Left "User already exists."
		Right True  -> Right $ key userBs

deactivateUser :: Key User -> MultiDb ()
deactivateUser = undefined

activateUser :: Key User -> MultiDb ()
activateUser = undefined

authenticateUser :: Text -> Text -> MultiDb Bool
authenticateUser username password = do
	mUser <- redis $ get (namespace userKeys <> username)
	case mUser of
		Left r -> Left $ "Redis error: " <> (pack $ show r)
		Right Nothing  -> return False
		Right (Just u) -> case decode u of
			Nothing -> 

getUser :: Key User -> MultiDb (Maybe User)
getUser k = do
	mUser <- redis $ get (namespace userKeys <> fromKey k)
	return $! case mUser of
		Left r -> Nothing
		Right bs -> bs >>= decode . fromStrict

updateUser :: Key User -> Changeset User -> MultiDb User
updateUser = undefined


