{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.ListHandlers where
import Control.Monad.Trans
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
createList = do
	timestamp <- now
	userKey <- currentUserId
	list <- jsonData
	withData (L.createList $ initializeList timestamp userKey list) (json . fmap asList)

getList :: Handler c ()
getList = do
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
getUserLists = do
	userId <- currentUserId
	withData (L.getUserLists userId) (json . map (fmap asList))
