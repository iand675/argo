{-# LANGUAGE OverloadedStrings #-}
module Zippy.Accounts.Data.Types where
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Zippy.Base.Common (Key, Namespace(..))

instance Namespace User where
	namespace = const "user"

instance Namespace Group where
	namespace = const "group"

data Group = Group
	{ groupName :: Text
	, groupOwner :: Key User
	, groupActive :: Bool
	} deriving (Read, Show, Eq)

data User = User
	{ userUsername         :: Text
	, userName             :: Text
	, userAvatar           :: Maybe Text
	, userEmail            :: Text
	, userPasswordHash     :: ByteString
	, userStripeCustomerId :: Maybe Text
	, userCompany          :: Maybe Text
	, userCreatedAt        :: UTCTime
	} deriving (Read, Show, Eq)

data Membership = Membership
	{ membershipRole :: MembershipRole
	, membershipGroup :: Key Group
	, membershipHistory :: [MembershipHistory]
	} deriving (Read, Show, Eq)

data UserMemberships = UserMemberships
	{ userMembershipsUser :: Key User
	, userMembershipsMemberships :: [Membership]
	} deriving (Read, Show, Eq)

data MembershipRole = Admin
					| Moderator
					| Member
					deriving (Read, Show, Eq)

data MembershipHistory = MembershipCreated { membershipHistoryTimestamp :: UTCTime }
					   | MembershipDeleted { membershipHistoryTimestamp :: UTCTime }
					   | MembershipChanged { membershipHistoryTimestamp :: UTCTime
					                       , membershipHistoryRole :: MembershipRole }
					   deriving (Read, Show, Eq)