{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.Tasks.Data.List where
import Data.Aeson
import qualified Zippy.Accounts.Domain.Types as Domain
import Zippy.Accounts.Data.Relationships (ownerIx, creatorIx)
import Zippy.Base.Common
import Zippy.Base.Data
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import Zippy.Tasks.Data.Relationships
import Zippy.Tasks.Data.Types
import qualified Zippy.Tasks.Domain.Types as Domain

instance O.AsContent List where
    fromContent = decode . C.value
    value = encode
    contentType = const O.jsonContent
    indexes l = ownerIx (rekey $ listOwner l) : creatorIx (rekey $ listCreator l) : []

createList :: Domain.List -> MultiDb (Entity Domain.List)
createList l = do
    r <- riak $ fmap Right $ putNew list () $ toData l
    k <- ifNothing AlreadyExists $ C.key r
    e <- ifNothing DeserializationError $ C.putResponseVClock r
    return $! Entity (Key k) e l

getList :: Key Domain.List -> MultiDb (Entity Domain.List)
getList k = do
    r <- riak $ fmap Right $ get list () $ rekey k
    rawList <- justOne $ C.getContent r
    dbList <- ifNothing DeserializationError $ O.fromContent rawList
    e <- ifNothing DeserializationError $ C.getVClock r
    return $! Entity k e $ fromData dbList
    --return $! fmap result $ (ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) gr
    --where result = Entity k . fromData

getGroupLists :: Key Domain.Group -> MultiDb [Entity Domain.List]
getGroupLists k = do
    keys <- riak $ fmap Right $ indexEq list "group" $ fromKey k
    mapM (getList . rekey) keys

--do
    --keys <- indexEq list (namespace group) (fromKey k)
    --values <- mapM (get list ()) keys
    --return $! mapMaybe (ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) values
    --lists <- linkWalk k [(namespace list, Nothing, True)] list 

getUserLists :: Key Domain.User -> MultiDb [Entity Domain.List]
getUserLists k = do
    keys <- riak $ fmap Right $ indexEq list "owner" $ fromKey k
    mapM (getList . rekey) keys
    --do
    --keys <- indexEq list (namespace user) (fromKey k)
    --values <- mapM (get list ()) keys
    --return $! mapMaybe (ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) values

--updateList :: Key List -> Changeset List -> O.Riak List
--updateList = undefined

archiveList :: Key List -> MultiDb ()
archiveList _ = undefined