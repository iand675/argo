{-# LANGUAGE TemplateHaskell #-}
module Zippy.User.Model.User where
import Prelude hiding (id)
import Zippy.Base.Model

data User = User
	{ id :: Text
	, name :: Text
	, avatar :: Text
	, email :: Text
	} deriving (Read, Show, Eq)

deriveJSON id' ''User