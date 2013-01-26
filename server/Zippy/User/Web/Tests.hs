module Zippy.User.Web.Tests where
import Data.Maybe
import Test.Hspec
import Test.Hspec.HUnit
import Test.QuickCheck
import Test.HUnit
import Zippy.Base.Client
import qualified Zippy.User.Client as C

local = runClient "http://127.0.0.1:3000"

isLeft (Left _) = True
isLeft _ = False

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
	it "should return 400 Bad Request if a required field for the model is not set" $ do
		pending "not implemented"
	it "should return 201 Created when the user is created" $ do
		pending "not implemented"
	it "should set the cookie when the user is created" $ do
		pending "not implemented"
	it "should require an email address" $ do
		pending "not implemented"
	it "should require a password" $ do
		pending "not implemented"
	it "should require a name" $ do
		pending "not implemented"
	it "should require a username" $ do
		pending "not implemented"

getUser :: Spec
getUser = describe "GET /v1/users/:user" $ do
	it "should return not found if the user doesn't exist" $ do
		pending "not implemented"

getCurrentUser :: Spec
getCurrentUser = describe "GET /v1/users/me" $ do
	it "should return 200 if the user is authenticated" $ do
		result <- local C.getCurrentUser
		assertBool (show result) $ (result /= Right Nothing) && not (isLeft result)
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
