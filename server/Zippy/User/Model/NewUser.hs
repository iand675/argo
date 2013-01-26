{-# LANGUAGE TemplateHaskell #-}
module Zippy.User.Model.NewUser where
import Prelude hiding (id)
import Zippy.Base.Model

data NewUser = NewUser
	{ name :: Text
	, email :: Text
	, password :: Text
	, confirmPassword :: Text
	, mailable :: Bool
	}

deriveJSON id' ''NewUser