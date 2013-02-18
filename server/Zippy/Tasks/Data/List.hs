{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.Tasks.Data.List where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Zippy.Base.Common
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import Zippy.Tasks.Data.Group
import Zippy.User.Data.User (User, user)

instance Namespace List where
    namespace = const "list"

listIx :: Key List -> (ByteString, Maybe ByteString)
listIx = keyIndex "list"

data List = List
    { name :: Text
    , owner :: Key User
    }

instance O.AsContent List where
    fromContent = const Nothing

list :: Proxy List
list = Proxy

createList :: List -> O.Riak b (Maybe (Key List))
createList l = do
   pr <- putNew list () l
   return $! fmap Key $ C.key pr

getList :: Key List -> O.Riak b (Maybe List)
getList k = do
    gr <- get list () k
    return $! (O.fromContent <=< justOne . C.getContent) gr

getGroupLists :: Key Group -> O.Riak b [{- TODO: Entity -} List]
getGroupLists k = do
    keys <- indexEq list (namespace group) (fromKey k)
    values <- mapM (get list ()) keys
    return $! mapMaybe (O.fromContent <=< justOne . C.getContent) values
    --lists <- linkWalk k [(namespace list, Nothing, True)] list 

getUserLists :: Key User -> O.Riak b [{- TODO: Entity -} List]
getUserLists k = do
    keys <- indexEq list (namespace user) (fromKey k)
    values <- mapM (get list ()) keys
    return $! mapMaybe (O.fromContent <=< justOne . C.getContent) values

--updateList :: Key List -> Changeset List -> O.Riak List
--updateList = undefined

archiveList :: Key List -> O.Riak b ()
archiveList = undefined