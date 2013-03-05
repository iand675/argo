{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans
import qualified Data.Text.Lazy.IO as T
import Web.Scotty hiding (file, param, header, html)
import Zippy.Base.Data (DbConfig, initialize, shutdown)
import Zippy.Base.Web
import Zippy.Tasks.Web.GroupHandlers
import Zippy.Tasks.Web.ListHandlers
import Zippy.Tasks.Web.TaskHandlers
import Zippy.Tasks.Web.UserHandlers
import Zippy.User.Session

data Config = Config
	{ dbConfig :: DbConfig
	}

sendApp = authenticate landingPage appPage
	where
		landingPage = do
			header "Content-Type" "text/html"
			file "./content/bs-app.html"
			header "Cache-Control" "no-cache"
			header "Expires" "Tue, 01 Jan 1980 1:00:00 GMT"
			header "Pragma" "no-cache"
		appPage _ = do
			header "Content-Type" "text/html"
			liftIO (T.readFile "./content/bs-app.html") >>= html
			header "Cache-Control" "no-cache"
			header "Expires" "Tue, 01 Jan 1980 1:00:00 GMT"
			header "Pragma" "no-cache"

sendFile = do
	f <- param "filename"
	file $ "./content/" ++ f

routes :: Config -> ScottyM ()
routes conf = do
	let withConf = handle conf
	get    "/groups"              $ withConf listGroups
	post   "/groups"              $ withConf createGroup
	get    "/groups/:group"       $ withConf getGroup
	post   "/groups/:group"       $ withConf updateGroup
	post   "/groups/:group/join"  $ withConf undefined
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
	get    "/users/me/lists"      $ withConf getUserLists
	get    "/users/me/groups"     $ withConf undefined
	get    "/users/:user"         $ withConf getUser
	get    "/users/:user/groups"  $ withConf listUserGroups
	post   "/signin"              $ withConf signIn
	--post   "/signout"             $ withConf signOut
	----
	get    "/"                    $ withConf sendApp
	get    "/content/:filename"   $ withConf sendFile
	notFound                      $ Web.Scotty.raise "not found"

main = do
	dbConf <- initialize
	scotty 3000 $ routes $ Config dbConf
	shutdown dbConf
