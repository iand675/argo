{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Model.Task where
import Zippy.Base.Model
import Zippy.Tasks.Model.Sideload
import Zippy.User.Model.User

data Task = Task
	{ id :: Text
	, name :: Text
	, description :: Text
	, tags :: [Text]
	, creator :: User
	, subtasks :: Maybe [Sideload Task]
	, assignedTo :: User
	, history :: [()]
	}

deriveJSON id' ''Task