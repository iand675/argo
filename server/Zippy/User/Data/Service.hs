{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Data.Service where
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Text (Text, pack)
import Data.Text.Encoding
import Database.Redis hiding (decode)
import Zippy.Base.Data
import Zippy.User.Data.User
import qualified Zippy.User.Domain.Models as Domain

instance Namespace User where
    namespace = const "user:"

createUser :: Domain.User -> MultiDb (Either Text (Key Domain.User))
createUser u = do
    --metric "user:create"
    let userBs = encodeUtf8 $ username u
    userCreated <- redis $ setnx (namespace userKeys <> userBs) $ toStrict $ encode u
    return $! case userCreated of
        Left r      -> Left $ "Redis error: " <> (pack $ show r)
        Right False -> Left "User already exists."
        Right True  -> Right $ key userBs

getUser :: Key Domain.User -> MultiDb (Maybe Domain.User)
getUser k = do
    mUser <- redis $ get (namespace userKeys <> fromKey k)
    return $! case mUser of
        Left r -> Nothing
        Right bs -> bs >>= decode . fromStrict

updateUser :: Key Domain.User -> Domain.User -> MultiDb Domain.User
updateUser = undefined


