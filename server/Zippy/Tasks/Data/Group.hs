{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses, EmptyDataDecls #-}
module Zippy.Tasks.Data.Group where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Either
import Data.Proxy
import Data.Time
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Model
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import qualified Zippy.Tasks.Domain.Models as Domain
import qualified Zippy.User.Domain.Models as Domain
import qualified Zippy.User.Data.User as Data

domainToData :: Domain.Group -> Group
domainToData g = Group
	{ name = Domain.groupName g
	, owner = rekey $ Domain.groupOwner g
	, members = map rekey $ Domain.groupMembers g
	, active = Domain.groupActive g
	}

group :: Proxy Group
group = Proxy

data User

instance Namespace User where
	namespace = const "user"

instance Namespace Group where
	namespace = const "group"

ownerLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
ownerLink = link "owner"

memberLink :: Key User -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
memberLink = link "member"

data Group = Group
	{ name :: Text
	, owner :: Key Domain.User
	, members :: [Key Domain.User]
	, active :: Bool
	} deriving (Read, Show, Eq)

deriveJSON id ''Group

instance O.AsContent Group where
	fromContent = decode . C.value
	value = encode
	links u = (ownerLink $ rekey $ owner u) : map (memberLink . rekey) (members u)
	contentType = const O.jsonContent

instance DataRep Domain.Group Group where
	fromData g = Domain.Group
		{ Domain.groupName = name g
		, Domain.groupOwner = rekey $ owner g
		, Domain.groupMembers = map rekey $ members g
		, Domain.groupActive = active g
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

getUserGroups :: Key Domain.User -> MultiDb [Entity Domain.Group]
getUserGroups k = do
	mu <- Data.getUser $ rekey k
	case mu of
		Left err -> return $ Left err
		Right u -> do
			mgs <- mapM (getGroup . rekey) $ Data.memberships u
			return $! Right $ rights mgs

updateGroup :: Key Domain.Group -> Domain.Group -> MultiDb (Entity Domain.Group)
updateGroup = undefined

archiveGroup :: Key Domain.Group -> MultiDb (Entity Domain.Group)
archiveGroup k = do
	mg <- getGroup k
	case mg of
		l@(Left _) -> return l
		Right g -> updateGroup k $ (value g) { Domain.groupActive = False }