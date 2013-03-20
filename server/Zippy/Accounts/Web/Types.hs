{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.Accounts.Web.Types where
import Zippy.Base.Common
import Zippy.Base.JSON
import Data.Proxy

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

data SignInRequest = SignInRequest
    { signInUsername :: Text
    , signInPassword :: Text
    } deriving (Read, Show, Eq)

data NewGroup = NewGroup
    { newGroupName    :: Text
    , newGroupMembers :: Maybe [Key User]
    } deriving (Read, Show, Eq)

data GroupChangeset = GroupChangeset
    { groupChangesetName  :: Maybe Text
    , groupChangesetOwner :: Maybe (Key User)
    } deriving (Read, Show, Eq)

data Group = Group
    { groupName    :: Text
    , groupOwner   :: Key User
    , groupMembers :: [Key User]
    --, groupLists   :: [Sideload List]
    } deriving (Read, Show, Eq)

deriveJSON (stripPrefix 1) ''CurrentUser
deriveJSON (stripPrefix 1) ''UserChangeset
deriveJSON (stripPrefix 1) ''NewUser
deriveJSON (stripPrefix 0) ''User
deriveJSON (stripPrefix 2) ''SignInRequest
deriveJSON (stripPrefix 1) ''NewGroup
deriveJSON (stripPrefix 1) ''GroupChangeset
deriveJSON (stripPrefix 0) ''Group

user :: Proxy User
user = Proxy

currentUser :: Proxy CurrentUser
currentUser = Proxy

group :: Proxy Group
group = Proxy