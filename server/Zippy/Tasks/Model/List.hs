{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Model.List where
import Zippy.Base.Model
import Zippy.Tasks.Model.Task
import Zippy.User.Model.User (User)

data List = List
	{ id :: Text
	, name :: Text
	, creator :: User
	, administrators :: [User]
	-- , group :: Group
	-- , createdDate :: DateTime
	-- , modifiedDate :: DateTime
	, icon :: Text
	, tasks :: [Task]
	}

deriveJSON id' ''List