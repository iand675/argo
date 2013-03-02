{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.Group.Data.Group where
import Zippy.Base.Common
import Zippy.Base.Model hiding (Key)
import Zippy.Riak.Object hiding (Key)
import qualified Zippy.Riak.Content as C
import Zippy.User.Data.User

data Group = Group
	{ groupName :: Text
	, groupOwner :: Key User
	, groupMembers :: [Key User]
	}

jsonize 0 ''Group

instance AsContent Group where
	fromContent = decode . C.value
	value = encode
	links c = ownerLink (groupOwner c) : map memberLink (groupMembers c)
	indexes c = 