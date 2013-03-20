module Zippy.GroupTests where
import Test.Hspec
import Test.QuickCheck
import Test.HUnit

groupTests :: Spec
groupTests = describe "Group Tests" $ do
	listGroups
	createGroup
	getGroup

listGroups :: Spec
listGroups = describe "GET /v1/groups" $ do
	it "should return 401 Unauthorized if the user is not authenticated" $ do
		pending "authentication not implemented"
	it "should return 200 OK if the user is authenticated" $ do
		pending "authentication not implemented"
	describe "with no parameters" $ do
		it "should return all groups that a user is a member of" $ do
			pending "group backend not implemented"
		it "should not return groups that a user has left or been removed from" $ do
			pending "group backend not implemented"

createGroup :: Spec
createGroup = describe "POST /v1/groups" $ do
	it "should return 401 Unauthorized if the user is not authenticated" $ do
		pending "authentication not implemented"
	it "should return 201 Created if the user is authenticated and has available groups for his plan" $ do
		pending "group backend not implemented"
	it "should set Location header to /v1/groups/:group on creation" $ do
		pending "route not implemented"
	it "should return 400 Bad Request if a required field for the model is not set" $ do
		pending "models not implemented"

getGroup :: Spec
getGroup = describe "GET /v1/groups/:group" $ do
	it "should return 401 Unauthorized if the user is not authenticated or a member" $ do
		pending "thinking about problems"
	it "should return 200 OK if the user is a member and the group is found" $ do
		pending "stuff"
