{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.User.Web.Models where
import Zippy.Base.Model

data CurrentUser = CurrentUser
    { currentUserUsername     :: Text
    , currentUserName         :: Text
    , currentUserAvatar       :: Maybe Text
    , currentUserEmail        :: Text
    } deriving (Read, Show, Eq)

data UserChangeset = UserChangeset
    { userChangesetName     :: Maybe Text
    , userChangesetAvatar   :: Maybe (Maybe Text)
    , userChangesetEmail    :: Maybe Text
    , userChangesetPassword :: Maybe Text
    , userChangesetCompany  :: Maybe (Maybe Text)
    } deriving (Read, Show, Eq)

data NewUser = NewUser
    { newUserUsername        :: Text
    , newUserName            :: Text
    , newUserEmail           :: Text
    , newUserPassword        :: Text
    } deriving (Read, Show, Eq)

data User = User
    { userUsername :: Text
    , userName     :: Text
    , userAvatar   :: Maybe Text
    , userEmail    :: Text
    } deriving (Read, Show, Eq)

instance Changeset CurrentUser UserChangeset where
    apply = undefined

deriveJSON (stripPrefix 1) ''CurrentUser
deriveJSON (stripPrefix 1) ''UserChangeset
deriveJSON (stripPrefix 0) ''NewUser
deriveJSON (stripPrefix 0) ''User