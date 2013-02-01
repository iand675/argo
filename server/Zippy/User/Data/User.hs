{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.User.Data.User where
import Data.Proxy
import Data.Time
import Zippy.Base.Model

userKeys = Proxy :: Proxy User

data User = User
	{ username         :: Text
	, name             :: Text
	, avatar           :: Maybe Text
	, email            :: Text
	, passwordHash     :: Text
	, stripeCustomerId :: Maybe Text
	, company          :: Maybe Text
	, createdAt        :: UTCTime
	} deriving (Read, Show, Eq)

deriveJSON id ''User

