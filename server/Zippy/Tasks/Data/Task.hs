{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.Tasks.Data.Task where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe
import Data.Proxy
import Data.Time
import Zippy.Accounts.Data.Relationships (User, creatorIx, assignedToIx)
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.JSON
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import Zippy.Tasks.Data.Relationships
import Zippy.Tasks.Data.Types
import qualified Zippy.Tasks.Domain.Types as Domain

instance O.AsContent Task where
    fromContent = decode . C.value
    value = encode
    contentType = const O.jsonContent

createTask :: Domain.Task -> MultiDb (Entity Domain.Task)
createTask t = do
    newTask <- riak $ fmap Right $ putNew task () $ toData t
    k <- ifNothing AlreadyExists $ C.key newTask
    e <- ifNothing DeserializationError $ C.putResponseVClock newTask
    return $! Entity (Key k) e t

getTask :: Key Domain.Task -> MultiDb (Entity Domain.Task)
getTask k = do
    c <- riak $ fmap Right $ get task () $ rekey k
    aContent <- justOne $ C.getContent c
    rawTask <- ifNothing DeserializationError $ O.fromContent aContent
    e <- ifNothing DeserializationError $ C.getVClock c
    return $! Entity k e $ fromData rawTask

getListTasks :: Key Domain.List -> MultiDb [Entity Domain.Task]
getListTasks k = undefined 
{- riak $ do
    -- TODO: once linkwalking / mapreduce is better understood,
    -- consider replacing indexing with something like this:
    --gr <- linkWalk k [(namespace task, Nothing, True)] list
    keys <- indexEq task (namespace list) (fromKey k)
    values <- mapM (get task ()) keys
    -- TODO: do something better than filtering out invalid values
    return $! mapMaybe (O.fromContent <=< justOne . C.getContent) values
    -}
