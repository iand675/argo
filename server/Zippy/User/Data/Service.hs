{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Data.Service where
import Data.Aeson
import qualified Data.ByteString as S
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Monoid
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Encoding
import Database.Redis hiding (decode)
import Zippy.Base.Common
import Zippy.Base.Data
import qualified Zippy.User.Data.User as Raw
import qualified Zippy.User.Domain.Models as Domain

domainToData :: Domain.User -> Raw.User
domainToData u = Raw.User
    { Raw.username         = Domain.userUsername u
    , Raw.name             = Domain.userName u
    , Raw.avatar           = Domain.userAvatar u
    , Raw.email            = Domain.userEmail u
    , Raw.passwordHash     = Domain.userPasswordHash u
    , Raw.stripeCustomerId = Domain.userStripeCustomerId u
    , Raw.company          = Domain.userCompany u
    , Raw.createdAt        = Domain.userCreatedAt u
    , Raw.memberships      = []
    }

entitize :: a -> Key b -> Entity a
entitize x k = Entity (rekey k) x

createUser :: Domain.User -> MultiDb (Entity Domain.User)
createUser u = fmap combine $ Raw.createUser $ domainToData u
    where combine = fmap (entitize u)

getUser :: Key Domain.User -> MultiDb (Entity Domain.User)
getUser u = fmap combine $ Raw.getUser rawKey
    where
        combine = fmap (Entity u . fromData)
        rawKey :: Key Raw.User
        rawKey = rekey u

signIn :: Text -> Text -> MultiDb Bool
signIn = Raw.signIn

--redisKey :: Proxy a -> Key a -> ByteString
--redisKey p k = toStrict (namespace p <> ":" <> fromKey k)

--createUser :: Domain.User -> MultiDb (Either Text (Key Domain.User))
--createUser u = do
--    --metric "user:create"
--    let userBs = encodeUtf8 $ username u
--    userCreated <- redis $ setnx (toStrict $ redisKey user $ toStrict $ encode u
--    return $! case userCreated of
--        Left r      -> Left $ "Redis error: " <> (pack $ show r)
--        Right False -> Left "User already exists."
--        Right True  -> Right $ key userBs

--getUser :: Key Domain.User -> MultiDb (Maybe Domain.User)
--getUser k = do
--    mUser <- redis $ get (namespace user <> ":" <> fromKey k)
--    return $! case mUser of
--        Left r -> Nothing
--        Right bs -> bs >>= decode . fromStrict

--updateUser :: Key Domain.User -> Domain.User -> MultiDb Domain.User
--updateUser = undefined


