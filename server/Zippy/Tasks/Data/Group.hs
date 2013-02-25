{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.Tasks.Data.Group where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Proxy
import Data.Time
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Model
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import qualified Zippy.Tasks.Domain.Models as Domain
import Zippy.User.Data.User (User)

domainToData :: Domain.Group -> Group
domainToData g = Group
	{ name = Domain.groupName g
	, owner = rekey $ Domain.groupOwner g
	, members = map rekey $ Domain.groupMembers g
	}

group :: Proxy Group
group = Proxy

instance Namespace Group where
	namespace = const "group"

ownerLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
ownerLink = link "owner"

memberLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
memberLink = link "member"

data Group = Group
	{ name :: Text
	, owner :: Key User
	, members :: [Key User]
	}

deriveJSON id ''Group

instance O.AsContent Group where
	fromContent = decode . C.value
	value = encode
	links u = (ownerLink $ owner u) : map memberLink (members u)
	contentType = const O.jsonContent

instance DataRep Domain.Group Group where
	fromData g = Domain.Group
		{ Domain.groupName = name g
		, Domain.groupOwner = rekey $ owner g
		, Domain.groupMembers = map rekey $ members g
		}

createGroup :: Domain.Group -> MultiDb (Key Domain.Group)
createGroup g = riak $ do
	pr <- putNew group () $ domainToData g
	return $ case C.key pr of
		Nothing -> Left DataConflict
		Just k -> Right $ Key k

getGroup :: Key Domain.Group -> MultiDb (Entity Domain.Group)
getGroup k = riak $ do
	gr <- get group () $ rekey k
	return $ fmap (Entity k . fromData) $
		(ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) gr

getGroups :: Key a -> MultiDb [Entity Domain.Group]
getGroups = undefined

-- updateGroup :: Key Group -> Changeset Group -> O.Riak b Group
-- updateGroup = undefined

archiveGroup :: Key Group -> O.Riak b ()
archiveGroup = undefined