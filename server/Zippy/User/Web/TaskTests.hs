module TaskTests where
import Test.Hspec
import Test.QuickCheck
import Test.HUnit

taskTests :: Spec
taskTests = describe "Task Tests" $ do
	getTask
	updateTask
	deleteTask

getTask = describe "GET /v1/tasks/:task" $ do
	it "whatevers" $ pending "whatever"

updateTask = describe "POST /v1/tasks/:task" $ do
	it "whatevers" $ pending "whatever"

deleteTask = describe "DELETE /v1/tasks/:task" $ do
	it "whatevers" $ pending "whatever"
