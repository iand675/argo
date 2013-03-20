{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Zippy.Accounts.Data.Types where
import Data.ByteString.Char8 (ByteString)
import Data.Time (UTCTime)
import qualified Zippy.Accounts.Domain.Types as Domain
import Zippy.Base.Common (rekey, Key, Namespace(..), DomainData(..), DataDomain(..))
import Zippy.Base.JSON
import qualified Zippy.Riak.Types as Riak

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

data Group = Group
	{ groupName :: Text
	, groupOwner :: Key User
	, groupActive :: Bool
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

data Membership = Membership
	{ membershipRole :: MembershipRole
	, membershipGroup :: Key Group
	, membershipHistory :: [MembershipHistory]
	} deriving (Read, Show, Eq)

data UserMemberships = UserMemberships
	{ userMembershipsUser :: Key User
	, userMembershipsMemberships :: [Membership]
	} deriving (Read, Show, Eq)

deriveJSON (stripPrefix 0) ''User
deriveJSON (stripPrefix 0) ''Group
deriveJSON id ''MembershipRole
deriveJSON (stripPrefix 0) ''Membership
deriveJSON (stripPrefix 1) ''MembershipHistory
deriveJSON (stripPrefix 1) ''UserMemberships

instance Namespace User where
	namespace = const "user"

instance Namespace Group where
	namespace = const "group"

instance Riak.Namespace User where
	namespace = const "user"

instance Riak.Namespace Group where
	namespace = const "group"

instance DataDomain User Domain.User where
	fromData u = Domain.User
	    { Domain.userUsername         = userUsername u
	    , Domain.userName             = userName u
	    , Domain.userAvatar           = userAvatar u
	    , Domain.userEmail            = userEmail u
	    , Domain.userPasswordHash     = userPasswordHash u
	    , Domain.userStripeCustomerId = userStripeCustomerId u
	    , Domain.userCompany          = userCompany u
	    , Domain.userCreatedAt        = userCreatedAt u
	    }

instance DomainData Domain.User User where
	toData u = User
	    { userUsername         = Domain.userUsername u
	    , userName             = Domain.userName u
	    , userAvatar           = Domain.userAvatar u
	    , userEmail            = Domain.userEmail u
	    , userPasswordHash     = Domain.userPasswordHash u
	    , userStripeCustomerId = Domain.userStripeCustomerId u
	    , userCompany          = Domain.userCompany u
	    , userCreatedAt        = Domain.userCreatedAt u
	    }

instance DataDomain Group Domain.Group where
	fromData g = Domain.Group
		{ Domain.groupName = groupName g
		, Domain.groupOwner = rekey $ groupOwner g
		, Domain.groupActive = groupActive g
		, Domain.groupMembers = []
		}

instance DomainData Domain.Group Group where
	toData g = Group
		{ groupName = Domain.groupName g
		, groupOwner = rekey $ Domain.groupOwner g
		, groupActive = Domain.groupActive g
		}

