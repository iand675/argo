{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Web.Tests where
import Data.Maybe
import qualified Data.Text as T
import System.Random
import Test.Hspec
import Test.Hspec.HUnit
import Test.QuickCheck
import Test.HUnit
import Zippy.Base.Client
import Zippy.Base.Common
import qualified Zippy.User.Client as C
import Zippy.User.Web.Models

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

userTests :: Spec
userTests = describe "User Tests" $ do
	listUsers
	createUser
	getUser
	getCurrentUser
	listUserGroups

listUsers :: Spec
listUsers = describe "GET /v1/users" $ do
	return ()

createUser :: Spec
createUser = describe "POST /v1/users" $ do
	it "should return 201 Created when the user is created" $ do
		result <- local . C.createUser =<< randomUser
		assertBool (show result) $ (result /= Right Nothing) && not (isLeft result)
	it "should set the cookie when the user is created" $ do
		pending "not implemented"
	it "should require an email address" $ do
		result <- local . C.createUser . (\u -> u { newUserEmail = "" }) =<< randomUser
		assertBool (show result) $ result == Right Nothing
	it "should require a password" $ do
		result <- local . C.createUser . (\u -> u { newUserPassword = "" }) =<< randomUser
		assertBool (show result) $ result == Right Nothing
	it "should require a name" $ do
		result <- local . C.createUser . (\u -> u { newUserName = "" }) =<< randomUser
		assertBool (show result) $ result == Right Nothing
	it "should require a username" $ do
		result <- local . C.createUser . (\u -> u { newUserUsername = "" }) =<< randomUser
		assertBool (show result) $ result == Right Nothing

getUser :: Spec
getUser = describe "GET /v1/users/:user" $ do
	it "should return the user if the user does exist" $ do
		(Right (Just newUser)) <- local . C.createUser =<< randomUser
		result <- local $ C.getUser $ toKey $ currentUserUsername newUser
		assertBool (show result) $ (result /= Right Nothing) && not (isLeft result)
	it "should return not found if the user doesn't exist" $ do
		result <- local $ C.getUser $ toKey $ "GARBAGE_WAY_TOO_LONG_USER_NAME_DOESNT_EXIST"
		assertBool (show result) $ result == Right Nothing

getCurrentUser :: Spec
getCurrentUser = describe "GET /v1/users/me" $ do
	it "should return 200 if the user is authenticated" $ do
		result <- local C.getCurrentUser
		assertBool (show result) $ (result /= Right Nothing) && not (isLeft result)
	it "should return the current user if the user is authenticated" $ do
		pending "not implemented"
	it "should return 404 if unauthenticated" $ do
		result <- local C.getCurrentUser
		assertEqual "no current user" (Right Nothing) result

listUserGroups :: Spec
listUserGroups = describe "GET /v1/users/:user/groups" $ do
	it "should return all of the public groups that another user is in" $ do
		pending "not implemented"
	it "should return all groups that the authenticated user is in" $ do
		pending "not implemented"
	it "should return not found if the user doesn't exist" $ do
		pending "not implemented"
	it "should return an empty list if the user is not a member of a group" $ do
		(Right (Just newUser)) <- local . C.createUser =<< randomUser
		result <- local $ C.listUserGroups $ toKey $ currentUserUsername newUser
		assertBool (show result) $ fmap (fmap null) result == Right (Just True)
