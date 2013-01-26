{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Zippy.Tasks.Web.GroupHandlers
import Zippy.Tasks.Web.ListHandlers
import Zippy.Tasks.Web.TaskHandlers
import Zippy.Tasks.Web.UserHandlers

routes :: ScottyM ()
routes = do
	get    "/v1/groups"              listGroups
	post   "/v1/groups"              createGroup
	get    "/v1/groups/:group"       getGroup
	post   "/v1/groups/:group"       updateGroup
	delete "/v1/groups/:group"       deleteGroup
	get    "/v1/groups/:group/lists" getGroupLists
	post   "/v1/groups/:group/lists" createGroupList
	get    "/v1/groups/:group/users" getGroupUsers
	post   "/v1/groups/:group/users" addUserToGroup
	----
	get    "/v1/lists"               getLists
	get    "/v1/lists/:list"         getList
	post   "/v1/lists/:list"         updateList
	delete "/v1/lists/:list"         deleteList
	get    "/v1/lists/:list/tasks"   getListTasks
	post   "/v1/lists/:list/tasks"   createListTask
	----
	--get    "/v1/search"              search
	----
	get    "/v1/tasks/:task"         getTask
	post   "/v1/tasks/:task"         updateTask
	delete "/v1/tasks/:task"         deleteTask
	----
	get    "/v1/users"               listUsers
	post   "/v1/users"               createUser
	get    "/v1/users/me"            getCurrentUser
	get    "/v1/users/:user"         getUser
	get    "/v1/users/:user/groups"  listUserGroups
	----
	notFound                       $ raise "not found"

main = scotty 3000 routes