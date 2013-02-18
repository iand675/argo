{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.User.Data.User where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Proxy
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Data.Time
import Zippy.Base.Common
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

createUser :: User -> O.Riak b (Maybe (Key User))
createUser u = do
	let k = toKey $ username u
	gr <- get user () k
	case C.getContent gr of
		[] -> do
			pr <- put user (O.withVClockFrom gr) k u
			return $ Just k
		_ -> return Nothing

getUser :: Key User -> O.Riak b (Maybe User)
getUser k = do
	gr <- get user () k
	case C.getContent gr of
		[] -> return Nothing
		(u:[]) -> return $ O.fromContent u
		_ -> return Nothing

