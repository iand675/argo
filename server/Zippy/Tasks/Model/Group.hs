{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Model.Group where
import Zippy.Base.Model
import Zippy.Tasks.Model.List
import Zippy.Tasks.Model.Sideload
import Zippy.User.Model.User

data Group = Group
	{ id :: Text
	, name :: Text
	, owner :: User
	, members :: [Sideload User]
	, lists :: [Sideload List]
	}

deriveJSON id' ''Group