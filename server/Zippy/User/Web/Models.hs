{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.User.Models where
import Zippy.Base.Model

data CurrentUser = CurrentUser
    { currentUserUsername     :: Text
    , currentUserName         :: Text
    , currentUserAvatar       :: Text
    , currentUserEmail        :: Text
    , currentUserPasswordHash :: Text
    , currentUserMailable     :: Bool
    } deriving (Read, Show, Eq)

data UserChangeset = UserChangeset
    { userChangesetName     :: Maybe Text
    , userChangesetAvatar   :: Maybe Text
    , userChangesetEmail    :: Maybe Text
    , userChangesetPassword :: Maybe Text
    , userChangesetMailable :: Maybe Bool
    } deriving (Read, Show, Eq)

data NewUser = NewUser
    { newUserName            :: Text
    , newUserEmail           :: Text
    , newUserPassword        :: Text
    , newUserConfirmPassword :: Text
    , newUserMailable        :: Bool
    } deriving (Read, Show, Eq)

data User = User
    { userId     :: Text
    , userName   :: Text
    , userAvatar :: Text
    , userEmail  :: Text
    } deriving (Read, Show, Eq)

instance Changeset CurrentUser UserChangeset where
    apply = undefined

deriveJSON (stripPrefix 1) ''CurrentUser
deriveJSON (stripPrefix 1) ''UserChangeset
deriveJSON (stripPrefix 0) ''NewUser
deriveJSON (stripPrefix 0) ''User