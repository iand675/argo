{-# LANGUAGE OverloadedStrings, TemplateHaskell, EmptyDataDecls #-}
module Zippy.Accounts.Data.User where
import Crypto.PasswordStore
import qualified Data.ByteString as B
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

user :: Proxy User
user = Proxy

ownerLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
ownerLink = link "owner"

memberLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
memberLink = link "member"

ownerIx :: Key User -> (ByteString, Maybe ByteString)
ownerIx = keyIndex "owner_bin"

creatorIx :: Key User -> (ByteString, Maybe ByteString)
creatorIx = keyIndex "creator_bin"

assignedToIx :: Key User -> (ByteString, Maybe ByteString)
assignedToIx = keyIndex "assigned_to_bin"

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
signIn :: Text -> Text -> MultiDb (Maybe Text)
signIn key password = do
	mUser <- getUser $ toKey key
	case mUser of
		Left _ -> return $ Right Nothing
		Right u -> if verifyPassword (encodeUtf8 password) (passwordHash u)
			then return $ Right $ Just $ username u
			else return $ Right Nothing