{-# LANGUAGE OverloadedStrings #-}
module Zippy.ListTests where
import Control.Monad.Trans
import qualified Data.Text as T
import System.Random
import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Zippy.Base.Client
import Zippy.Base.Common
import Zippy.Accounts.Web.Types
import Zippy.Tasks.Web.Types
import qualified Zippy.Accounts.Client as C
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

aList :: NewList
aList = NewList
	{ newListName = "Test List"
	, newListAdministrators = Nothing
	, newListGroup = Nothing
	, newListIcon = Nothing
	}

listTests :: Spec
listTests = describe "List Tests" $ do
	getLists
	getList
	updateList
	deleteList
	getListTasks
	createListTask

nonexistantList :: Key List
nonexistantList = Key "wombat"

getLists = describe "GET /lists" $ do
	return ()

getList = describe "GET /lists/:list" $ do
	it "returns 404 when the list doesn't exist" $ do
		result <- local $ C.getList nonexistantList
		assertBool (show result) $ result == Right Nothing
	it "returns 200 when the list exists" $ do
		aUser <- randomUser
		result <- local $ do
			(Right (Just u)) <- C.createUser aUser
			newList <- C.createList aList
			liftIO $ assertBool (show newList) $ (not $ isLeft newList) && (newList /= Right Nothing)
			let (Right (Just e)) = newList
			C.getList $ rekey $ key e
		assertBool (show result) $ (not $ isLeft result) && (result /= Right Nothing)
	it "returns 401 unauthorized when the user isn't authenticated" $ do
		aUser <- randomUser
		result <- local $ do
			(Right (Just u)) <- C.createUser aUser
			C.createList aList
		assertBool (show result) $ (not $ isLeft result) && (result /= Right Nothing)
		let (Right (Just e)) = result
		listResult <- local $ C.getList $ key e
		assertBool (show listResult) $ listResult == Right Nothing

updateList = describe "POST /lists/:list" $ do
	return ()

deleteList = describe "DELETE /lists/:list" $ do
	return ()

getListTasks = describe "GET /lists/:list/tasks" $ do
	return ()

createListTask = describe "POST /lists/:list/tasks" $ do
	return ()
