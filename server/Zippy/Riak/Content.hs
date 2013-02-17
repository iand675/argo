module Zippy.Riak.Content where
import Data.ByteString.Lazy (ByteString)
import Data.Word

data Quorum = One
            | Quorum
            | All
            | Default
            | Count Int
            deriving (Read, Show, Eq)

encodeQuorum :: Integral a => Quorum -> a
encodeQuorum One       = 0xFFFFFFFE
encodeQuorum Quorum    = 0xFFFFFFFD
encodeQuorum All       = 0xFFFFFFFC
encodeQuorum Default   = 0xFFFFFFFB
encodeQuorum (Count x) = fromIntegral x

data Content a = Content
    { value             :: ByteString
    , links             :: [(Maybe ByteString, Maybe ByteString, Maybe ByteString)]
    , userMetadata      :: [(ByteString, Maybe ByteString)]
    , indexes           :: [(ByteString, Maybe ByteString)]
    , characterSet      :: Maybe ByteString
    , contentType       :: Maybe ByteString
    , contentEncoding   :: Maybe ByteString
    , vtag              :: Maybe ByteString
    , lastModified      :: Maybe Word32
    , lastModifiedUsecs :: Maybe Word32
    , deleted           :: Maybe Bool
    } deriving (Read, Show, Eq)

data GetRequest = GetRequest
    { getBucket            :: ByteString
    , getKey               :: ByteString
    , getReadQuorum        :: Maybe Quorum
    , getPrimaryReadQuorum :: Maybe Quorum
    , getBasicQuorum       :: Maybe Bool
    , getNotFoundOK        :: Maybe Bool
    , getIfModified        :: Maybe ByteString
    , getHead              :: Maybe Bool
    , getDeletedVClock     :: Maybe Bool
    } deriving (Read, Show, Eq)

data GetResponse a = GetResponse
    { getContent :: [Content a]
    , getVClock  :: Maybe ByteString
    , unchanged  :: Maybe Bool
    } deriving (Read, Show, Eq)

data PutRequest a = PutRequest
    { putBucket             :: ByteString
    , putKey                :: Maybe ByteString
    , putRequestVClock      :: Maybe ByteString
    , putRequestContent     :: Content a
    , putWriteQuorum        :: Maybe Quorum
    , putDurableWriteQuorum :: Maybe Quorum
    , putReturnBody         :: Maybe Bool
    , putPrimaryWriteQuorum :: Maybe Quorum
    , putIfNotModified      :: Maybe Bool
    , putIfNoneMatch        :: Maybe Bool
    , putReturnHead         :: Maybe Bool
    } deriving (Read, Show, Eq)

data PutResponse a = PutResponse
    { putResponseContent :: [Content a]
    , putResponseVClock  :: Maybe ByteString
    , key                :: Maybe ByteString
    } deriving (Read, Show, Eq)

data DeleteRequest = DeleteRequest
    { deleteBucket             :: ByteString
    , deleteKey                :: ByteString
    , deleteVClock             :: Maybe ByteString
    , deleteQuorum             :: Maybe Quorum
    , deleteReadQuorum         :: Maybe Quorum
    , deleteWriteQuorum        :: Maybe Quorum
    , deletePrimaryReadQuorum  :: Maybe Quorum
    , deletePrimaryWriteQuorum :: Maybe Quorum
    , deleteDurableWriteQuorum :: Maybe Quorum
    } deriving (Read, Show, Eq)
