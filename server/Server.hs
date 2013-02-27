{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty
import Zippy.Base.Data (DbConfig, initialize, shutdown)
import Zippy.Base.Web
import Zippy.Tasks.Web.GroupHandlers
import Zippy.Tasks.Web.ListHandlers
import Zippy.Tasks.Web.TaskHandlers
import Zippy.Tasks.Web.UserHandlers

data Config = Config
	{ dbConfig :: DbConfig
	}

routes :: Config -> ScottyM ()
routes conf = do
	let withConf = handle conf
	--get    "/groups"              $ withConf listGroups
	post   "/groups"              $ withConf createGroup
	get    "/groups/:group"       $ withConf getGroup
	post   "/groups/:group"       $ withConf updateGroup
	--delete "/groups/:group"       $ withConf archiveGroup
	--get    "/groups/:group/lists" $ withConf getGroupLists
	--post   "/groups/:group/lists" $ withConf createGroupList
	--get    "/groups/:group/users" $ withConf getGroupUsers
	--post   "/groups/:group/users" $ withConf addUserToGroup
	----
	get    "/lists"               $ withConf listLists
	post   "/lists"               $ withConf createList
	get    "/lists/:list"         $ withConf getList
	post   "/lists/:list"         $ withConf updateList
	delete "/lists/:list"         $ withConf archiveList
	get    "/lists/:list/tasks"   $ withConf getListTasks
	--post  "lists/:list/tasks"   $ withConf createListTask
	----
	get    "/tasks"               $ withConf listTasks
	post   "/tasks"               $ withConf createTask
	get    "/tasks/:task"         $ withConf getTask
	post   "/tasks/:task"         $ withConf updateTask
	delete "/tasks/:task"         $ withConf archiveTask
	---
	get    "/users"               $ withConf listUsers
	post   "/users"               $ withConf createUser
	get    "/users/me"            $ withConf getCurrentUser
	get    "/users/:user"         $ withConf getUser
	get    "/users/:user/lists"   $ withConf getUserLists
	get    "/users/:user/groups"  $ withConf listUserGroups
	post   "/signin"              $ withConf signIn
	----
	notFound                      $ Web.Scotty.raise "not found"

main = do
	dbConf <- initialize
	scotty 3000 $ routes $ Config dbConf
	shutdown dbConf
