{-# LANGUAGE MultiParamTypeClasses #-}
module Zippy.Accounts.Domain.Types where
--import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Text
import Data.Time
import Zippy.Base.Common

--instance Changeset User Web.UserChangeset where
--    apply u cs = Right $ u
--        { userName         = update userName Web.userChangesetName
--        , userAvatar       = update userAvatar Web.userChangesetAvatar
--        , userEmail        = update userEmail Web.userChangesetEmail
--        , userPasswordHash = update userPasswordHash (fmap hashPassword . Web.userChangesetPassword)
--        , userCompany      = update userCompany Web.userChangesetCompany
--        }
--        where
--            update :: (User -> a) -> (Web.UserChangeset -> Maybe a) -> a
--            update field change = maybe (field u) id $ change cs


data User = User
    { userUsername         :: Text
    , userName             :: Text
    , userAvatar           :: Maybe Text
    , userEmail            :: Text
    , userPasswordHash     :: ByteString
    , userStripeCustomerId :: Maybe Text
    , userCompany          :: Maybe Text
    , userCreatedAt        :: UTCTime
--   , userPlan             :: Subscription
    } deriving (Read, Show, Eq)

data Group = Group
    { groupName    :: Text
    , groupOwner   :: Key User
    , groupMembers :: [Key User]
    , groupActive  :: Bool
    } deriving (Read, Show, Eq)

