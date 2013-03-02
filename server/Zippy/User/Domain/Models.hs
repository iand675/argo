{-# LANGUAGE MultiParamTypeClasses #-}
module Zippy.User.Domain.Models where
import Control.Monad
import Data.Time
import Zippy.Base.Model
import Zippy.Base.Data (DataRep(..))
import qualified Zippy.User.Data.User as Data
import qualified Zippy.User.Web.Models as Web

hashPassword = id

instance Changeset User Web.UserChangeset where
    apply u cs = Right $ u
        { userName         = update userName Web.userChangesetName
        , userAvatar       = update userAvatar Web.userChangesetAvatar
        , userEmail        = update userEmail Web.userChangesetEmail
        , userPasswordHash = update userPasswordHash (fmap hashPassword . Web.userChangesetPassword)
        , userCompany      = update userCompany Web.userChangesetCompany
        }
        where
            update :: (User -> a) -> (Web.UserChangeset -> Maybe a) -> a
            update field change = maybe (field u) id $ change cs

instance DataRep User Data.User where
    fromData du = User
        { userUsername         = Data.username du
        , userName             = Data.name du
        , userAvatar           = Data.avatar du
        , userEmail            = Data.email du
        , userPasswordHash     = Data.passwordHash du
        , userStripeCustomerId = Data.stripeCustomerId du
        , userCompany          = Data.company du
        , userCreatedAt        = Data.createdAt du
        }

initializeUser :: Web.NewUser -> UTCTime -> User
initializeUser n t = User
    { userUsername         = Web.newUserUsername n
    , userName             = Web.newUserName n
    , userAvatar           = Nothing
    , userEmail            = Web.newUserEmail n
    , userPasswordHash     = hashPassword $ Web.newUserPassword n
    , userStripeCustomerId = Nothing
    , userCompany          = Nothing
    , userCreatedAt        = t
    }

asCurrentUser :: User -> Web.CurrentUser
asCurrentUser u = Web.CurrentUser (userUsername u) (userName u) (userAvatar u) (userEmail u)

asUser :: User -> Web.User
asUser u = Web.User (userUsername u) (userName u) (userAvatar u) (userEmail u)

data User = User
    { userUsername         :: Text
    , userName             :: Text
    , userAvatar           :: Maybe Text
    , userEmail            :: Text
    , userPasswordHash     :: Text
    , userStripeCustomerId :: Maybe Text
    , userCompany          :: Maybe Text
    , userCreatedAt        :: UTCTime
--   , userPlan             :: Subscription
    } deriving (Read, Show, Eq)

