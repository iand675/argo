module Zippy.Riak.Simple where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Proxy
import Zippy.Base.Common
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O

--singleOrNothing :: C.GetResponse a -> Maybe a
--singleOrNothing r = case C.getContent r of
--	[] -> Nothing
--	(x:[]) -> O.fromContent x
--	_ -> Nothing

get :: (Namespace a, O.Get c, O.AsContent a) => Proxy a -> c -> Key a -> O.Riak b (C.GetResponse a)
get p s k = O.get $ O.createGetRequest (namespace p) (fromKey k) s

put :: (Namespace a, O.Put c, O.AsContent a) => Proxy a -> c -> Key a -> a -> O.Riak b (C.PutResponse a)
put p s k c = O.put $ O.createPutRequest (namespace p) (Just $ fromKey k) s c

putNew :: (Namespace a, O.Put c, O.AsContent a) => Proxy a -> c -> a -> O.Riak b (C.PutResponse a)
putNew p s c = O.put $ O.createPutRequest (namespace p) Nothing s c

delete :: (Namespace a, O.Delete c) => Proxy a -> c -> Key a -> O.Riak b ()
delete p s k = O.delete $ O.createDeleteRequest (namespace p) (fromKey k) s

class Indexable a where
    indexEq :: (Namespace n) => Proxy n -> ByteString -> a -> O.Riak b [Key n]
    indexRange :: (Namespace n) => Proxy n -> ByteString -> a -> a -> O.Riak b [Key n]

instance Indexable ByteString where
    indexEq p ix val = fmap (map Key) $ O.index (namespace p) ix $ O.StringQuery $ O.Key val
    indexRange p ix low high = fmap (map Key) $ O.index (namespace p) ix $ O.StringQuery $ O.Range low high
