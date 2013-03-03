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
createGroup = authenticate (status unauthorized401) $ \userKey -> do
	groupDto <- jsonData
	let newGroup = initializeGroup userKey groupDto
	withData (G.createGroup $ initializeGroup userKey groupDto) $ \g -> do
		json $ Entity (rekey g) (asGroup newGroup)

getGroup :: Handler c ()
getGroup = authenticate (status unauthorized401) $ \userKey -> do
	groupId <- group
	withData (G.getGroup groupId) $ \g -> do
		if userKey == (groupOwner $ value g) || (any (== userKey) $ groupMembers $ value g)
			then json $ fmap asGroup g
			else status notFound404

updateGroup :: Handler c ()
updateGroup = do
	raise "not ready yet"
	--groupId <- group
	--changeset <- jsonData
	--withData (fmap applyChangeset (G.getGroup groupId) >>= G.updateGroup groupId)

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

