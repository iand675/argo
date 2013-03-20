{-# LANGUAGE OverloadedStrings, TemplateHaskell, EmptyDataDecls #-}
module Zippy.Accounts.Data.User where
import Crypto.PasswordStore
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding
import Zippy.Accounts.Data.Relationships
import Zippy.Accounts.Data.Types
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Riak.Simple
import qualified Zippy.Riak.Object as O
import qualified Zippy.Riak.Content as C

instance O.AsContent User where
	fromContent = decode . C.value
	value = encode
	contentType = const O.jsonContent

instance O.Put User where
	writeQuorum = const $ Just C.All
instance O.Get User
instance O.Delete User

createUser :: User -> MultiDb (Entity User)
createUser u = riak $ do
	let k = rekey $ toKey $ userUsername u
	gr <- get user () $ k
	case C.getContent gr of
		[] -> do
			pr <- put user (O.withVClockFrom gr) k u
			case C.putResponseVClock pr of
				Nothing -> return $ Left DeserializationError
				Just e -> return $ Right $ Entity (rekey k) e u
		_ -> return $ Left AlreadyExists

getUser :: Key User -> MultiDb (Entity User)
getUser k = do
	gr <- riak $ fmap Right $ get user () $ rekey k
	dbUser <- justOne (C.getContent gr)
	u <- ifNothing DeserializationError $ O.fromContent dbUser
	e <- ifNothing DeserializationError $ C.getVClock gr
	return $ Entity k e u

-- username -> password -> bool
signIn :: Text -> Text -> MultiDb (Maybe Text)
signIn username password = do
	mUser <- onFailure (const $ return Nothing) (return . Just) $ getUser $ rekey $ toKey username
	case mUser of
		Nothing -> return Nothing
		Just u -> if verifyPassword (encodeUtf8 password) (userPasswordHash $ value u)
			then return $ Just $ userUsername $ value u
			else return Nothing