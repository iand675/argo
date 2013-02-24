{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Zippy.Riak.Object where
import Control.Monad.Operational
import qualified Zippy.Riak.Content as C
import Data.ByteString.Lazy.Char8 (ByteString)

type Bucket = ByteString
type Key = ByteString
type Tag = ByteString
type Index = ByteString

data HTTP  = HTTP
data Proto = Proto
data Adaptive = Adaptive

class HttpTransport a where {}
instance HttpTransport HTTP where {}
instance HttpTransport Adaptive where {}

class ProtoTransport a where {}
instance ProtoTransport Proto where {}
instance ProtoTransport Adaptive where {}

--applyVClock :: (ReceivedVClock a, UseVClock b) => a -> b -> b
--applyVClock = useVClock . vclock

class ReceivedVClock a where
    receiveVClock :: a -> Maybe ByteString

instance ReceivedVClock (C.Content a) where
    receiveVClock = C.vtag

instance ReceivedVClock (C.GetResponse a) where
    receiveVClock = C.getVClock

instance ReceivedVClock (C.PutResponse a) where
    receiveVClock = C.putResponseVClock

--class UseVClock a where
--    useVClock :: Maybe ByteString -> a -> a

--instance UseVClock C.GetRequest where
--    useVClock vc x = x { C.getIfModified = vc }

--instance UseVClock (C.PutRequest a) where
--    useVClock vc x = x { C.putRequestVClock = vc }

--instance UseVClock C.DeleteRequest where
--    useVClock vc x = x { C.deleteVClock = vc }

class AsContent a where
    fromContent :: C.Content a -> Maybe a

    toContent :: a -> C.Content a
    toContent a = C.Content
        { C.value             = value a
        , C.links             = links a
        , C.userMetadata      = metadata a
        , C.indexes           = indexes a
        , C.characterSet      = characterSet a
        , C.contentType       = contentType a
        , C.contentEncoding   = contentEncoding a
        , C.vtag              = Nothing
        , C.lastModified      = Nothing
        , C.lastModifiedUsecs = Nothing
        , C.deleted           = Nothing
        }

    value :: a -> ByteString
    value = const ""

    links :: a -> [(Maybe Bucket, Maybe Key, Maybe Tag)]
    links = const []

    metadata :: a -> [(ByteString, Maybe ByteString)]
    metadata = const []

    indexes :: a -> [(ByteString, Maybe ByteString)]
    indexes = const []

    characterSet :: a -> Maybe ByteString
    characterSet = const Nothing

    contentType :: a -> Maybe ByteString
    contentType = const Nothing

    contentEncoding :: a -> Maybe ByteString
    contentEncoding = const Nothing

class Get a where
    readQuorum        :: a -> Maybe C.Quorum
    readQuorum = const Nothing

    primaryReadQuorum :: a -> Maybe C.Quorum
    primaryReadQuorum = const Nothing

    basicQuorum       :: a -> Maybe Bool
    basicQuorum = const Nothing

    notFoundOK        :: a -> Maybe Bool
    notFoundOK = const Nothing

    ifModified        :: a -> Maybe ByteString
    ifModified = const Nothing

    deletedVClock     :: a -> Maybe Bool
    deletedVClock = const Nothing

    headOnly          :: a -> Maybe Bool
    headOnly = const Nothing

instance Get ()

class Put a where
    writeQuorum :: a -> Maybe C.Quorum
    writeQuorum = const Nothing

    durableWriteQuorum :: a -> Maybe C.Quorum
    durableWriteQuorum = const Nothing

    returnBody :: a -> Maybe Bool
    returnBody = const Nothing

    primaryWriteQuorum :: a -> Maybe C.Quorum
    primaryWriteQuorum = const Nothing

    ifNotModified :: a -> Maybe Bool
    ifNotModified = const Nothing

    ifNoneMatch :: a -> Maybe Bool
    ifNoneMatch = const Nothing

    vclock :: a -> Maybe ByteString
    vclock = const Nothing

    returnHead :: a -> Maybe Bool
    returnHead = const Nothing

class Delete a where
    deleteQuorum :: a -> Maybe C.Quorum
    deleteQuorum = const Nothing

    deleteReadQuorum :: a -> Maybe C.Quorum
    deleteReadQuorum = const Nothing

    deleteWriteQuorum :: a -> Maybe C.Quorum
    deleteWriteQuorum = const Nothing

    deletePrimaryReadQuorum :: a -> Maybe C.Quorum
    deletePrimaryReadQuorum = const Nothing

    deletePrimaryWriteQuorum :: a -> Maybe C.Quorum
    deletePrimaryWriteQuorum = const Nothing

    deleteDurableWriteQuorum :: a -> Maybe C.Quorum
    deleteDurableWriteQuorum = const Nothing

instance Delete ()

createGetRequest :: Get a => Bucket -> Key -> a -> C.GetRequest
createGetRequest b k x = C.GetRequest
    { C.getBucket            = b
    , C.getKey               = k
    , C.getReadQuorum        = readQuorum x
    , C.getPrimaryReadQuorum = primaryReadQuorum x
    , C.getBasicQuorum       = basicQuorum x
    , C.getNotFoundOK        = notFoundOK x
    , C.getIfModified        = ifModified x
    , C.getHead              = headOnly x
    , C.getDeletedVClock     = deletedVClock x
    }

createPutRequest :: (Put v, AsContent a) => Bucket -> Maybe Key -> v -> a -> C.PutRequest a
createPutRequest b mk p x = C.PutRequest
    { C.putBucket             = b
    , C.putKey                = mk
    , C.putRequestVClock      = vclock p
    , C.putRequestContent     = toContent x
    , C.putWriteQuorum        = writeQuorum p
    , C.putDurableWriteQuorum = durableWriteQuorum p
    , C.putReturnBody         = returnBody p
    , C.putPrimaryWriteQuorum = primaryWriteQuorum p
    , C.putIfNotModified      = ifNotModified p
    , C.putIfNoneMatch        = ifNoneMatch p
    , C.putReturnHead         = returnHead p
    }

createDeleteRequest :: Delete a => Bucket -> Key -> a -> C.DeleteRequest
createDeleteRequest b k x = C.DeleteRequest
    { C.deleteBucket             = b
    , C.deleteKey                = k
    , C.deleteVClock             = Nothing
    , C.deleteQuorum             = deleteQuorum x
    , C.deleteReadQuorum         = deleteReadQuorum x
    , C.deleteWriteQuorum        = deleteWriteQuorum x
    , C.deletePrimaryReadQuorum  = deletePrimaryReadQuorum x
    , C.deletePrimaryWriteQuorum = deletePrimaryWriteQuorum x
    , C.deleteDurableWriteQuorum = deleteDurableWriteQuorum x
    }

data WithVClock = WithVClock (Maybe ByteString)

withVClockFrom :: ReceivedVClock a => a -> WithVClock
withVClockFrom = WithVClock . receiveVClock

instance Put () 

instance Put WithVClock where
    vclock (WithVClock vc) = vc

data IndexQuery = StringQuery (QueryType ByteString)
                | IntQuery (QueryType Int)

data QueryType a = Key { key :: a }
                 | Range { rangeMin :: a, rangeMax :: a }

data RiakOp b a where
    GetOp :: C.GetRequest -> RiakOp b (C.GetResponse a)
    PutOp :: C.PutRequest a -> RiakOp b (C.PutResponse a)
    DeleteOp :: C.DeleteRequest -> RiakOp b ()
    IndexOp :: Bucket -> Index -> IndexQuery -> RiakOp b [Key]
    --MapReduce
    --Search
    --Status
    --GetBucket :: BucketProps b a => Bucket -> RiakOp b a
    ListBucketsOp :: RiakOp b [Bucket]
    --SetBucket
    GetClientIdOp :: ProtoTransport t => RiakOp t ByteString
    SetClientIdOp :: ProtoTransport t => ByteString -> RiakOp t ()
    ListKeysOp :: Bucket -> RiakOp b [Key]
    PingOp :: RiakOp b Bool

type RiakHTTP = Riak HTTP
type RiakProto = Riak Proto
newtype Riak b a = Riak { unwrapRiakProgram :: Program (RiakOp b) a }
    deriving (Functor, Monad)

get :: C.GetRequest -> Riak backend (C.GetResponse a)
get = Riak . singleton . GetOp

put :: C.PutRequest a -> Riak backend (C.PutResponse a)
put = Riak . singleton . PutOp

delete :: C.DeleteRequest -> Riak backend ()
delete = Riak . singleton . DeleteOp

index :: Bucket -> Index -> IndexQuery -> Riak backend [Key]
index b i q = Riak . singleton $ IndexOp b i q

listBuckets :: Riak backend [Bucket]
listBuckets = Riak $ singleton ListBucketsOp

getClientId :: ProtoTransport t => Riak t ByteString
getClientId = Riak $ singleton GetClientIdOp

setClientId :: ProtoTransport t => ByteString -> Riak t ()
setClientId = Riak . singleton . SetClientIdOp

listKeys :: Bucket -> Riak backend [Key]
listKeys = Riak . singleton . ListKeysOp

ping :: Riak backend Bool
ping = Riak $ singleton PingOp

jsonContent :: Maybe ByteString
jsonContent = Just "application/json"