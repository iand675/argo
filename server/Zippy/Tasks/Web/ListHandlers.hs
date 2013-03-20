{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.ListHandlers where
import Network.HTTP.Types.Status
import Zippy.Accounts.Session
import Zippy.Base.Common
import Zippy.Base.Web
import qualified Zippy.Tasks.Data.List as L
import Zippy.Tasks.Domain.Mappers
import Zippy.Tasks.Web.Types

listLists :: Handler c ()
listLists = raise "unimplemented"

createList :: Handler c ()
createList = authenticate (status unauthorized401) $ \userId -> do
	timestamp <- now
	listModel <- jsonData
	status created201
	withData (L.createList $ initializeList timestamp userId listModel) (json . fmap (toModel list))

getList :: Handler c ()
getList = authenticate (status unauthorized401) $ \_ {-userId-} -> do
	listId <- param "list"
	withData (L.getList $ Key listId) (json . fmap (toModel list))

updateList :: Handler c ()
updateList = raise "unimplemented"

archiveList :: Handler c ()
archiveList = do
	listId <- param "list"
	withData (L.archiveList $ Key listId) (const $ return ())

getListTasks :: Handler c ()
getListTasks = raise "unimplemented"

createListTask :: Handler c ()
createListTask = raise "unimplemented"

getUserLists :: Handler c ()
getUserLists = authenticate (status unauthorized401) $ \userId -> do
	withData (L.getUserLists userId) (json . map (fmap (toModel list)))

