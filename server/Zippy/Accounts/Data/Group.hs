{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses, EmptyDataDecls #-}
module Zippy.Accounts.Data.Group where
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Either
import Data.Proxy
import Data.Time
import Zippy.Accounts.Data.Relationships
import Zippy.Accounts.Data.Types
import Zippy.Accounts.Data.User
import qualified Zippy.Accounts.Domain.Types as Domain
import Zippy.Base.Common
import Zippy.Base.Data
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple

instance O.AsContent Group where
	fromContent = decode . C.value
	value = encode
	links u = [ownerLink $ rekey $ groupOwner u]
	contentType = const O.jsonContent

createGroup :: Domain.Group -> MultiDb (Entity Domain.Group)
createGroup g = riak $ do
	pr <- putNew group () $ toData g
	return $ case C.key pr of
		Nothing -> Left DataConflict
		Just k -> case C.putResponseVClock pr of
			Nothing -> Left DeserializationError
			Just e -> Right $ Entity (Key k) e g

getGroup :: Key Domain.Group -> MultiDb (Entity Domain.Group)
getGroup k = do
	gr <- riak $ fmap Right $ get group () $ rekey k
	rawGroup <- justOne $ C.getContent gr
	group <- ifNothing DeserializationError $ O.fromContent rawGroup
	e <- ifNothing DeserializationError $ C.getVClock gr
	return $! Entity k e $ fromData group

getUserGroups :: Key Domain.User -> MultiDb [Entity Domain.Group]
getUserGroups k = undefined
{-
	mu <- getUser $ rekey k
	case mu of
		Left err -> return $ Left err
		Right u -> do
			mgs <- mapM (getGroup . rekey) $ undefined
			return $! Right $ rights mgs
-}

updateGroup :: Key Domain.Group -> Domain.Group -> MultiDb (Entity Domain.Group)
updateGroup = undefined

archiveGroup :: Key Domain.Group -> MultiDb (Entity Domain.Group)
archiveGroup k = do
	g <- getGroup k
	updateGroup k $ (value g) { Domain.groupActive = False }