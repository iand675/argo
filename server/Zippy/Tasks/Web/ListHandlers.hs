{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.ListHandlers where
import Control.Monad.Trans
import Network.HTTP.Types.Status
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Web
import qualified Zippy.Tasks.Data.List as L
import Zippy.Tasks.Domain.Models
import Zippy.User.Domain.Models
import Zippy.User.Session

listLists :: Handler c ()
listLists = raise "unimplemented"

createList :: Handler c ()
createList = authenticate (status unauthorized401) $ \userId -> do
	timestamp <- now
	list <- jsonData
	status created201
	withData (L.createList $ initializeList timestamp userId list) (json . fmap asList)

getList :: Handler c ()
getList = authenticate (status unauthorized401) $ \userId -> do
	listId <- param "list"
	withData (L.getList $ Key listId) (json . fmap asList)

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
	withData (L.getUserLists userId) (json . map (fmap asList))
