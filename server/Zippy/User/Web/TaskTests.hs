{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Web.TaskTests where
import qualified Data.Text as T
import System.Random
import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.Tasks.Web.Models
import Zippy.User.Web.Models
import qualified Zippy.User.Client as C
import qualified Zippy.Tasks.Client as C

local = runClient "http://127.0.0.1:3000"

isLeft (Left _) = True
isLeft _ = False

randomUser :: IO NewUser
randomUser = do
	string <- sequence $ take 8 $ repeat $ randomRIO ('A', 'z')
	return $ NewUser
		{ newUserUsername = T.pack string
		, newUserName = "Test User"
		, newUserEmail = "test@example.com"
		, newUserPassword = "password"
		}

aTask = NewTask
	{ newTaskName = "Test Task"
	, newTaskDescription = Just "Test Description"
	, newTaskAssignedTo = Nothing
	}

taskTests :: Spec
taskTests = describe "Task Tests" $ do
	createTask
	getTask
	updateTask
	deleteTask

nonexistantTask :: Key Task
nonexistantTask = Key "wombat"

createTask = describe "POST /tasks" $ do
	it "returns 400 when the task is invalid" $ pending ""
	it "requires the user to be authenticated" $ pending ""
	it "returns the new task with appropriate key" $ do
		aUser <- randomUser
		result <- local $ do
			(Right (Just user)) <- C.createUser aUser
			C.createTask aTask
		assertBool (show result) $ (not $ isLeft result) && (result /= Right Nothing)

getTask = describe "GET /tasks/:task" $ do
	it "returns 404 when the task doesn't exist" $ do
		result <- local $ C.getTask nonexistantTask
		assertBool (show result) $ result == Right Nothing
	it "returns the task if the user owns it" $ pending ""
	it "returns the task if the user is in the group that owns it" $ pending ""
	it "returns 404 when the user is not allowed to see it" $ pending ""

updateTask = describe "POST /tasks/:task" $ do
	it "returns 404 when the task doesn't exist" $ pending ""
	it "returns 400 when the task cannot be deserialized" $ pending ""
	it "does not allow updates if the user does not have permission" $ pending ""

deleteTask = describe "DELETE /tasks/:task" $ do
	it "returns 404 when the task doesn't exist" $ do
		pending ""
		--aUser <- randomUser
		--result <- local $ do
		--	(Right (Just user)) <- C.createUser aUser
		--	C.deleteTask nonexistantTask
		--assertBool (show result) $ result == Right Nothing
	it "does not allow deletion if the user does not have permission" $ pending ""
	it "does not return the task after deletion" $ pending ""
