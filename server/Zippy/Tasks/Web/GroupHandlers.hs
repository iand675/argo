{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.GroupHandlers where
import Network.HTTP.Types.Status
import Zippy.Base.Common
import Zippy.Base.Web
import Zippy.User.Session
import Zippy.Tasks.Domain.Models
import qualified Zippy.Tasks.Data.Group as G

group = fmap Key $ param "group"

listGroups :: Handler c ()
listGroups = do
	return ()

createGroup :: Handler c ()
createGroup = do
	userKey <- currentUserId
	groupDto <- jsonData
	let newGroup = initializeGroup userKey groupDto
	withData (G.createGroup $ initializeGroup userKey groupDto) $ \g -> do
		json $ Entity (rekey g) (asGroup newGroup)

getGroup :: Handler c ()
getGroup = do
	groupId <- group
	withData (G.getGroup groupId) $ \g -> do
		userKey <- currentUserId
		if userKey == (groupOwner $ value g) || (any (== userKey) $ groupMembers $ value g)
			then json $ fmap asGroup g
			else status notFound404

--updateGroup :: Handler c ()
--updateGroup = do
--	groupId <- group

--archiveGroup :: Handler c ()
--archiveGroup = do
--	groupId <- group

--getGroupLists :: Handler c ()
--getGroupLists = do
--	groupId <- group

--createGroupList :: Handler c ()
--createGroupList = do
--	groupId <- group

--getGroupUsers :: Handler c ()
--getGroupUsers = do
--	groupId <- group

--addUserToGroup :: Handler c ()
--addUserToGroup = do
--	groupId <- group

--getUserLists :: Handler c ()
--getUserLists = do
--	userId <- currentUserId
--	--withData (G.getUserLists userId)
--	return ()

