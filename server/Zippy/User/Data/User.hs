{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.User.Data.User where
import Crypto.Hash.SHA512
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Encoding
import Data.Time.Clock (UTCTime)
import Data.Proxy
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Model
import Zippy.Riak.Simple
import qualified Zippy.Riak.Object as O
import qualified Zippy.Riak.Content as C

instance Namespace User where
	namespace = const "user"

user :: Proxy User
user = Proxy

ownerLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
ownerLink = link "owner"

memberLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
memberLink = link "member"

creatorIx :: Key User -> (ByteString, Maybe ByteString)
creatorIx = keyIndex "creator"

assignedToIx :: Key User -> (ByteString, Maybe ByteString)
assignedToIx = keyIndex "assignedTo"

data User = User
	{ username         :: Text
	, name             :: Text
	, avatar           :: Maybe Text
	, email            :: Text
	, passwordHash     :: Text
	, stripeCustomerId :: Maybe Text
	, company          :: Maybe Text
	, createdAt        :: UTCTime
	} deriving (Read, Show, Eq)

deriveJSON id ''User

instance O.AsContent User where
	fromContent = decode . C.value
	value = encode
	contentType = const O.jsonContent

instance O.Put User where
	writeQuorum = const $ Just C.All
instance O.Get User
instance O.Delete User

createUser :: User -> MultiDb (Key User)
createUser u = riak $ do
	let k = toKey $ username u
	gr <- get user () k
	case C.getContent gr of
		[] -> do
			pr <- put user (O.withVClockFrom gr) k u
			return $ Right k
		_ -> return $ Left AlreadyExists

getUser :: Key User -> MultiDb User
getUser k = riak $ do
	gr <- get user () k
	case C.getContent gr of
		[] -> return $ Left NotFound
		(u:[]) -> return $ ifNothing DeserializationError $ O.fromContent u
		_ -> return $ Left DataConflict

-- username -> password -> bool
signIn :: Text -> Text -> MultiDb Bool
signIn username password = do
	mUser <- getUser $ toKey username
	let hashedPassword = decodeUtf8 $ Base64.encode $ hash $ encodeUtf8 password
	return $ case mUser of
		Left _ -> Right False
		Right u -> Right (passwordHash u == hashedPassword)
