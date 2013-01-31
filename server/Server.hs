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
	get    "/v1/groups"              $ withConf listGroups
	post   "/v1/groups"              $ withConf createGroup
	get    "/v1/groups/:group"       $ withConf getGroup
	post   "/v1/groups/:group"       $ withConf updateGroup
	delete "/v1/groups/:group"       $ withConf deleteGroup
	get    "/v1/groups/:group/lists" $ withConf getGroupLists
	post   "/v1/groups/:group/lists" $ withConf createGroupList
	get    "/v1/groups/:group/users" $ withConf getGroupUsers
	post   "/v1/groups/:group/users" $ withConf addUserToGroup
	----
	get    "/v1/lists"               $ withConf getLists
	get    "/v1/lists/:list"         $ withConf getList
	post   "/v1/lists/:list"         $ withConf updateList
	delete "/v1/lists/:list"         $ withConf deleteList
	get    "/v1/lists/:list/tasks"   $ withConf getListTasks
	post   "/v1/lists/:list/tasks"   $ withConf createListTask
	----
	--get    "/v1/search"            $ withConf   search
	----
	get    "/v1/tasks/:task"         $ withConf getTask
	post   "/v1/tasks/:task"         $ withConf updateTask
	delete "/v1/tasks/:task"         $ withConf deleteTask
	---- 
	get    "/v1/users"               $ withConf listUsers
	post   "/v1/users"               $ withConf createUser
	get    "/v1/users/me"            $ withConf getCurrentUser
	get    "/v1/users/:user"         $ withConf getUser
	get    "/v1/users/:user/groups"  $ withConf listUserGroups
	----
	notFound                         $ Web.Scotty.raise "not found"

main = do
	dbConf <- initialize
	scotty 3000 $ routes $ Config dbConf
	shutdown dbConf