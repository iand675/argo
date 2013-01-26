module ListTests where
import Test.Hspec
import Test.QuickCheck
import Test.HUnit

listTests :: Spec
listTests = describe "List Tests" $ do
	getLists
	getList
	updateList
	deleteList
	getListTasks
	createListTask

getLists = describe "GET /v1/lists" $ do
	return ()

getList = describe "GET /v1/lists/:list" $ do
	return () 

updateList = describe "POST /v1/lists/:list" $ do
	return ()

deleteList = describe "DELETE /v1/lists/:list" $ do
	return ()

getListTasks = describe "GET /v1/lists/:list/tasks" $ do
	return ()

createListTask = describe "POST /v1/lists/:list/tasks" $ do
	return ()
