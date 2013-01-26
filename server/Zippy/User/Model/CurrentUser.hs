{-# LANGUAGE TemplateHaskell #-}
module Zippy.User.Model.CurrentUser where
import Prelude hiding (id)
import Zippy.Base.Model

data CurrentUser = CurrentUser
	{ id :: Text
	, name :: Text
	, avatar :: Text
	, email :: Text
	, passwordHash :: Text
	, mailable :: Bool
	} deriving (Read, Show, Eq)

deriveJSON id' ''CurrentUser