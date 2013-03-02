module Zippy.User.Web.TaskTests where
import Test.Hspec
import Test.QuickCheck
import Test.HUnit

taskTests :: Spec
taskTests = describe "Task Tests" $ do
	createTask
	getTask
	updateTask
	deleteTask

createTask = describe "POST /tasks" $ do
	it "returns 400 when the task is invalid" $ pending ""
	it "requires the user to be authenticated" $ pending ""

getTask = describe "GET /tasks/:task" $ do
	it "returns 404 when the task doesn't exist" $ pending ""
	it "returns the task if the user owns it" $ pending ""
	it "returns the task if the user is in the group that owns it" $ pending ""
	it "returns 404 when the user is not allowed to see it" $ pending ""

updateTask = describe "POST /tasks/:task" $ do
	it "returns 404 when the task doesn't exist" $ pending ""
	it "returns 400 when the task cannot be deserialized" $ pending ""
	it "does not allow updates if the user does not have permission" $ pending ""

deleteTask = describe "DELETE /tasks/:task" $ do
	it "returns 404 when the task doesn't exist" $ pending ""
	it "does not allow deletion if the user does not have permission" $ pending ""
	it "does not return the task after deletion" $ pending ""
