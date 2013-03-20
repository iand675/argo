{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.Tasks.Data.Task where
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
    return $! Entity (Key k) "" t

getTask :: Key Domain.Task -> MultiDb (Entity Domain.Task)
getTask k = do
    c <- riak $ fmap Right $ get task () $ rekey (rekey k :: Key Task)
    aContent <- justOne $ C.getContent c
    rawTask <- ifNothing DeserializationError $ O.fromContent aContent
    return $! Entity k "" $ fromData rawTask

getListTasks :: Key Domain.List -> MultiDb [Entity Domain.Task]
getListTasks _ = undefined 
{- riak $ do
    -- TODO: once linkwalking / mapreduce is better understood,
    -- consider replacing indexing with something like this:
    --gr <- linkWalk k [(namespace task, Nothing, True)] list
    keys <- indexEq task (namespace list) (fromKey k)
    values <- mapM (get task ()) keys
    -- TODO: do something better than filtering out invalid values
    return $! mapMaybe (O.fromContent <=< justOne . C.getContent) values
    -}
