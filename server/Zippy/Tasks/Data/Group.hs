{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.Tasks.Data.Group where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Proxy
import Data.Time
import Zippy.Base.Common
import Zippy.Base.Model
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import Zippy.User.Data.User (User)

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

createGroup :: Group -> O.Riak b (Maybe (Key Group))
createGroup g = do
	pr <- putNew group () g
	return $! fmap Key $ C.key pr

getGroup :: Key Group -> O.Riak b (Maybe Group)
getGroup k = do
	gr <- get group () k
	case C.getContent gr of
		[] -> return Nothing
		(u:[]) -> return $ O.fromContent u
		_ -> return Nothing

getGroups :: Key a -> O.Riak b [Group]
getGroups = undefined

-- updateGroup :: Key Group -> Changeset Group -> O.Riak b Group
-- updateGroup = undefined

archiveGroup :: Key Group -> O.Riak b ()
archiveGroup = undefined