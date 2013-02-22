{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Web.ListHandlers where
import Control.Monad.Trans
import Data.Time.Clock
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Web
import qualified Zippy.Tasks.Data.List as L
import Zippy.Tasks.Domain.Models

listLists :: Handler c ()
listLists = raise "unimplemented"

getList :: Handler c ()
getList = do
	listId <- param "list"
	ml <- runData $ L.getList $ Key listId
	case ml of
		Left issue -> handleDataError issue
		Right l -> json $ asList l

updateList :: Handler c ()
updateList = raise "unimplemented"

archiveList :: Handler c ()
archiveList = raise "unimplemented"

getListTasks :: Handler c ()
getListTasks = raise "unimplemented"

createListTask :: Handler c ()
createListTask = raise "unimplemented"
