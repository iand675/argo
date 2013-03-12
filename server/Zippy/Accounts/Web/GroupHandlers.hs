{-# LANGUAGE OverloadedStrings #-}
module Zippy.Accounts.Web.GroupHandlers where
import Data.Proxy
import Network.HTTP.Types.Status
import Zippy.Accounts.Domain.Mappers
import Zippy.Accounts.Web.Types
import Zippy.Accounts.Session
import qualified Zippy.Accounts.Data.Group as G
import Zippy.Base.Common
import Zippy.Base.Web

groupKey = fmap Key $ param "group"

listGroups :: Handler c ()
listGroups = do
	return ()

createGroup :: Handler c ()
createGroup = authenticate (status unauthorized401) $ \userKey -> do
	groupDto <- jsonData
	let newGroup = initializeGroup userKey groupDto
	withData (G.createGroup newGroup) $ \g -> do
		json $ fmap (toModel group) g

getGroup :: Handler c ()
getGroup = authenticate (status unauthorized401) $ \userKey -> do
	groupId <- groupKey
	withData (G.getGroup groupId) $ \dg -> do
		let g = fmap (toModel group) dg
		if userKey == (rekey $ groupOwner $ value g) || (any ((== userKey) . rekey) $ groupMembers $ value g)
			then json g
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

