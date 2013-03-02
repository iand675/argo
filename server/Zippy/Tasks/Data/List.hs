{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Zippy.Tasks.Data.List where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Either
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Base.Model
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import Zippy.Tasks.Data.Group hiding (User)
import qualified Zippy.Tasks.Domain.Models as Domain
import qualified Zippy.User.Domain.Models as Domain
import Zippy.User.Data.User (User, user)

domainToData :: Domain.List -> List
domainToData l = List
    { listName = Domain.listName l
    , listOwner = rekey $ Domain.listOwner l
    , listCreator = rekey $ Domain.listCreator l
    , listAdministrators = map rekey $ Domain.listAdministrators l
    , listGroup = fmap rekey $ Domain.listGroup l
    , listCreatedAt = Domain.listCreatedAt l
    , listIcon = Domain.listIcon l
    }

instance Namespace List where
    namespace = const "list"

listIx :: Key List -> (ByteString, Maybe ByteString)
listIx = keyIndex "list"

data List = List
    { listName :: Text
    , listOwner :: Key User
    , listCreator :: Key User
    , listAdministrators :: [Key User]
    , listGroup :: Maybe (Key Group)
    , listCreatedAt :: UTCTime
    , listIcon :: Maybe Text
    }

deriveJSON id ''List

instance O.AsContent List where
    fromContent = decode . C.value
    value = encode
    contentType = const O.jsonContent

instance DataRep Domain.List List where
    fromData l = Domain.List
        { Domain.listName = listName l
        , Domain.listOwner = rekey $ listOwner l
        , Domain.listCreator = rekey $ listCreator l
        , Domain.listAdministrators = map rekey $ listAdministrators l
        , Domain.listGroup = fmap rekey $ listGroup l
        , Domain.listCreatedAt = listCreatedAt l
        , Domain.listIcon = listIcon l
        }

list :: Proxy List
list = Proxy

createList :: Domain.List -> MultiDb (Entity Domain.List)
createList l = riak $ do
    pr <- putNew list () $ domainToData l
    return $! fmap ((flip Entity $ l) . Key) $ ifNothing AlreadyExists $ C.key pr

getList :: Key Domain.List -> MultiDb (Entity Domain.List)
getList k = riak $ do
    gr <- get list () $ rekey k
    return $! fmap result $ (ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) gr
    where result = Entity k . fromData

getGroupLists :: Key Domain.Group -> MultiDb [Entity Domain.List]
getGroupLists k = do
    mkeys <- riak $ fmap (Right . fmap rekey) $ indexEq list (namespace group) $ fromKey k
    case mkeys of
        Left err -> return $ Left err
        Right keys -> fmap (Right . rights) $ mapM getList keys

--do
    --keys <- indexEq list (namespace group) (fromKey k)
    --values <- mapM (get list ()) keys
    --return $! mapMaybe (ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) values
    --lists <- linkWalk k [(namespace list, Nothing, True)] list 

getUserLists :: Key Domain.User -> MultiDb [Entity Domain.List]
getUserLists k = do
    mkeys <- riak $ fmap (Right . fmap rekey) $ indexEq list (namespace user) $ fromKey k
    case mkeys of
        Left err -> return $ Left err
        Right keys -> fmap (Right . rights) $ mapM getList keys
    --do
    --keys <- indexEq list (namespace user) (fromKey k)
    --values <- mapM (get list ()) keys
    --return $! mapMaybe (ifNothing DeserializationError . O.fromContent <=< justOne . C.getContent) values

--updateList :: Key List -> Changeset List -> O.Riak List
--updateList = undefined

archiveList :: Key List -> MultiDb ()
archiveList k = undefined