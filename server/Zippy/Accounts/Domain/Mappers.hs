{-# LANGUAGE MultiParamTypeClasses #-}
module Zippy.Accounts.Domain.Mappers where
import Crypto.PasswordStore
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Zippy.Accounts.Domain.Types
import qualified Zippy.Accounts.Web.Types as Web
import Zippy.Base.Common

hashPassword :: Text -> IO ByteString
hashPassword t = makePassword (encodeUtf8 t) 12

initializeUser :: Web.NewUser -> UTCTime -> IO User
initializeUser n t = do
    hashedPassword <- hashPassword $ Web.newUserPassword n
    return $ User
        { userUsername         = Web.newUserUsername n
        , userName             = Web.newUserName n
        , userAvatar           = Nothing
        , userEmail            = Web.newUserEmail n
        , userPasswordHash     = hashedPassword
        , userStripeCustomerId = Nothing
        , userCompany          = Nothing
        , userCreatedAt        = t
        }

instance DomainModel User Web.CurrentUser where
    toModel _ u = Web.CurrentUser (userUsername u) (userName u) (userAvatar u) (userEmail u)

instance DomainModel User Web.User where
    toModel _ u = Web.User (userUsername u) (userName u) (userAvatar u) (userEmail u)

instance DomainModel Group Web.Group where
    toModel _ g = Web.Group
        { Web.groupName = groupName g
        , Web.groupOwner = rekey $ groupOwner g
        , Web.groupMembers = map rekey $ groupMembers g
        }

initializeGroup :: Key User -> Web.NewGroup -> Group
initializeGroup u g = Group
    { groupName = Web.newGroupName g
    , groupOwner = rekey u
    , groupMembers = map rekey $ maybe [] id $ Web.newGroupMembers g
    , groupActive = True
    }
