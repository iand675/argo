{-# LANGUAGE OverloadedStrings #-}
module Zippy.Accounts.Data.Service where
import Data.Aeson
import qualified Data.ByteString as S
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Monoid
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Encoding
import Database.Redis hiding (decode)
import Zippy.Accounts.Data.Types
import qualified Zippy.Accounts.Data.User as Raw
import qualified Zippy.Accounts.Domain.Types as Domain
import Zippy.Base.Common
import Zippy.Base.Data


entitize :: a -> Key b -> ByteString -> Entity a
entitize x k e = Entity (rekey k) e x

createUser :: Domain.User -> MultiDb (Entity Domain.User)
createUser = fmap (fmap fromData) . Raw.createUser . toData

getUser :: Key Domain.User -> MultiDb (Entity Domain.User)
getUser = fmap (fmap fromData) . Raw.getUser . rekey

signIn :: Text -> Text -> MultiDb (Maybe Text)
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


