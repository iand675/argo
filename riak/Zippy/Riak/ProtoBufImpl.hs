{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Zippy.Riak.ProtoBufImpl (
  runRiak,
  Riak,
  Pong(..),
  run,
  ping,
  getClientId,
  setClientId,
  getServerInfo,
  get,
  put,
  delete,
  listBuckets,
  listKeys,
  getBucket,
  setBucket,
  mapReduce,
  indexQuery,
  searchQuery,
  runProto
) where
import           Control.Applicative
import           Control.Exception
import           Control.Monad.Operational
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8                       as L
import qualified Data.Foldable                                    as F
import qualified Data.Sequence                                    as S
import           Data.Tagged
import           Prelude hiding (head)
import           Zippy.Riak.Connection
import qualified Zippy.Riak.Messages                              as M
import qualified Zippy.Riak.Types                                 as M
import qualified Zippy.Riak.Content                               as C
import qualified Zippy.Riak.Object                                as O
import           Zippy.Riak.Protocol.Content
import           Zippy.Riak.Protocol.DeleteRequest                as D
import           Zippy.Riak.Protocol.GetBucketRequest
import           Zippy.Riak.Protocol.GetBucketResponse
import           Zippy.Riak.Protocol.GetClientIDResponse
import qualified Zippy.Riak.Protocol.GetRequest                   as GR
import qualified Zippy.Riak.Protocol.GetResponse                  as GR
import           Zippy.Riak.Protocol.GetServerInfoResponse
import           Zippy.Riak.Protocol.IndexRequest                 as IR
import           Zippy.Riak.Protocol.IndexRequest.IndexQueryType  as IQ
import           Zippy.Riak.Protocol.IndexResponse
import qualified Zippy.Riak.Protocol.Link                         as L
import           Zippy.Riak.Protocol.ListBucketsResponse
import           Zippy.Riak.Protocol.ListKeysRequest
import qualified Zippy.Riak.Protocol.ListKeysResponse             as LK
import qualified Zippy.Riak.Protocol.MapReduceRequest             as MR
import qualified Zippy.Riak.Protocol.MapReduceResponse            as MR
import qualified Zippy.Riak.Protocol.Pair                         as P
import qualified Zippy.Riak.Protocol.PutRequest                   as PR
import qualified Zippy.Riak.Protocol.PutResponse                  as PS
import           Zippy.Riak.Protocol.SearchQueryRequest
import           Zippy.Riak.Protocol.SearchQueryResponse
import           Zippy.Riak.Protocol.SetBucketRequest
import           Zippy.Riak.Protocol.SetClientIDRequest           hiding (clientId)

class ToProtoRep a b | a -> b where
  toProtoRep :: a -> b

class FromProtoRep a b | a -> b where
  fromProtoRep :: b -> a

instance ToProtoRep C.Link L.Link where
  toProtoRep (C.Link b k t) = L.Link (Just b) (Just k) t

instance FromProtoRep C.Link L.Link where
  fromProtoRep l = C.Link b k (L.tag l)
    where
      unM f = maybe "" id $ f l
      b = unM L.bucket
      k = unM L.key

instance ToProtoRep (L.ByteString, Maybe L.ByteString) P.Pair where
  toProtoRep (a, b) = P.Pair a b

instance FromProtoRep (L.ByteString, Maybe L.ByteString) P.Pair where
  fromProtoRep (P.Pair a b) = (a, b)

instance ToProtoRep C.Index P.Pair where
  toProtoRep (C.Index i v) = P.Pair i (Just v)

instance FromProtoRep C.Index P.Pair where
  fromProtoRep (P.Pair i b) = C.Index i $ maybe "" id b

instance ToProtoRep (C.Content a) Content where
  toProtoRep c = Content
                 { value = C.value c
                 , links = S.fromList $ map toProtoRep $ C.links c
                 , usermeta = S.fromList $ map toProtoRep $ C.userMetadata c
                 , indexes = S.fromList $ map toProtoRep $ C.indexes c
                 , contentType = C.contentType c
                 , charset = C.characterSet c
                 , contentEncoding = C.contentEncoding c
                 , vtag = C.vtag c
                 , lastMod = C.lastModified c
                 , lastModUsecs = C.lastModifiedUsecs c
                 , deleted = C.deleted c
                 }

instance FromProtoRep (C.Content a) (Tagged a Content) where
  fromProtoRep c = C.Content
                   { C.value = value $ unTagged c
                   , C.links = map fromProtoRep $ F.toList $ links $ unTagged c
                   , C.userMetadata = map fromProtoRep $ F.toList $ usermeta $ unTagged c
                   , C.indexes = map fromProtoRep $ F.toList $ indexes $ unTagged c
                   , C.contentType = contentType $ unTagged c
                   , C.characterSet = charset $ unTagged c
                   , C.contentEncoding = contentEncoding $ unTagged c
                   , C.vtag = vtag $ unTagged c
                   , C.lastModified = lastMod $ unTagged c
                   , C.lastModifiedUsecs = lastModUsecs $ unTagged c
                   , C.deleted = deleted $ unTagged c
                   }


instance ToProtoRep C.GetRequest GR.GetRequest where
  toProtoRep c = GR.GetRequest
                 { GR.bucket = C.getBucket c
                 , GR.key = C.getKey c
                 , GR.r = fmap C.encodeQuorum $ C.getReadQuorum c
                 , GR.pr = fmap C.encodeQuorum $ C.getPrimaryReadQuorum c
                 , GR.basicQuorum = C.getBasicQuorum c
                 , GR.notfoundOk = C.getNotFoundOK c
                 , GR.ifModified = C.getIfModified c
                 , GR.head = C.getHead c
                 , GR.deletedvclock = C.getDeletedVClock c
                 }

instance FromProtoRep (C.GetResponse a) (Tagged a GR.GetResponse) where
-- toProtoRep c = undefined
  fromProtoRep c = C.GetResponse
                   { C.getContent = map (fromProtoRep . (\x -> Tagged x :: Tagged a Content)) $ F.toList $ GR.content $ unTagged c
                   , C.getVClock = GR.vclock $ unTagged c
                   , C.unchanged = GR.unchanged $ unTagged c
                   }

instance ToProtoRep (C.PutRequest a) PR.PutRequest where
  toProtoRep c = PR.PutRequest
                 { PR.bucket = C.putBucket c
                 , PR.key = C.putKey c
                 , PR.vclock = C.putRequestVClock c
                 , PR.content = toProtoRep $ C.putRequestContent c
                 , PR.w = fmap C.encodeQuorum $ C.putWriteQuorum c
                 , PR.dw = fmap C.encodeQuorum $ C.putDurableWriteQuorum c
                 , PR.pw = fmap C.encodeQuorum $ C.putPrimaryWriteQuorum c
                 , PR.returnBody = C.putReturnBody c
                 , PR.ifNotModified = C.putIfNotModified c
                 , PR.ifNoneMatch = C.putIfNoneMatch c
                 , PR.returnHead = C.putReturnHead c
                 }

instance FromProtoRep (C.PutResponse a) (Tagged a PS.PutResponse) where
  fromProtoRep c = C.PutResponse
                   { C.putResponseContent = map (fromProtoRep . (\x -> Tagged x :: Tagged a Content)) $ F.toList $ PS.content $ unTagged c
                   , C.putResponseVClock = PS.vclock $ unTagged c
                   , C.key = PS.key $ unTagged c
                   }

instance ToProtoRep C.DeleteRequest D.DeleteRequest where
  toProtoRep c = D.DeleteRequest
                 { D.bucket = C.deleteBucket c
                 , D.key = C.deleteKey c
                 , D.vclock = C.deleteVClock c
                 , D.rw = fmap C.encodeQuorum $ C.deleteQuorum c
                 , D.r = fmap C.encodeQuorum $ C.deleteReadQuorum c
                 , D.w = fmap C.encodeQuorum $ C.deleteWriteQuorum c
                 , D.pr = fmap C.encodeQuorum $ C.deletePrimaryReadQuorum c
                 , D.pw = fmap C.encodeQuorum $ C.deletePrimaryWriteQuorum c
                 , D.dw = fmap C.encodeQuorum $ C.deleteDurableWriteQuorum c
                 }

mapIndexRequest :: O.Bucket -> O.Index -> O.IndexQuery -> IR.IndexRequest
mapIndexRequest b i queryData = IndexRequest
                        { IR.bucket = b
                        , IR.index = i'
                        , IR.qtype = qt
                        , IR.key = k
                        , IR.rangeMin = minRange
                        , IR.rangeMax = maxRange
                        }
  where
    (i', qt, k, minRange, maxRange) = case queryData of
      O.StringQuery (O.Key k) -> (L.append i "_bin", IQ.Eq, Just k, Nothing, Nothing)
      O.StringQuery (O.Range low high) -> (L.append i "_bin", IQ.Range, Nothing, Just low, Just high)
      O.IntQuery (O.Key k) -> (L.append i "_int", IQ.Eq, Just undefined, Nothing, Nothing)
      O.IntQuery (O.Range low high) -> (L.append i "_int", IQ.Range, Nothing, Just undefined, Just undefined)

runRiak :: Connection -> Riak a -> IO a
runRiak c a = runReaderT (unwrapRiak a) c

newtype Riak a = Riak { unwrapRiak :: ReaderT Connection IO a }
               deriving (Monad, Applicative, Functor)

data Pong = Pong
          deriving (Read, Show, Eq)

run :: M.Request -> Riak M.Response
run x = Riak $ ask >>= liftIO . runRequest x

ping :: Riak Pong
ping = do
  resp <- run M.ping
  case resp of
    M.Pong -> return Pong
    _ -> throw $ M.UnexpectedResponse resp

getClientId :: Riak L.ByteString
getClientId = do
  resp <- run M.getClientId
  case resp of
    M.GetClientId resp -> return $ clientId resp
    _ -> throw $ M.UnexpectedResponse resp

setClientId :: L.ByteString -> Riak ()
setClientId bs = do
  resp <- run $ M.setClientId $ SetClientIDRequest bs
  case resp of
    M.SetClientId -> return ()
    _ -> throw $ M.UnexpectedResponse resp

getServerInfo :: Riak GetServerInfoResponse
getServerInfo = do
  resp <- run M.getServerInfo
  case resp of
    M.GetServerInfo r -> return r
    _ -> throw $ M.UnexpectedResponse resp

get :: GR.GetRequest -> Riak GR.GetResponse
get req = do
  resp <- run $ M.get req
  case resp of
    M.Get r -> return r
    _ -> throw $ M.UnexpectedResponse resp

put :: PR.PutRequest -> Riak PS.PutResponse
put req = do
  resp <- run $ M.put req
  case resp of
    M.Put r -> return r
    _ -> throw $ M.UnexpectedResponse resp

delete :: DeleteRequest -> Riak ()
delete req = do
  resp <- run $ M.delete req
  case resp of
    M.Delete -> return ()
    _ -> throw $ M.UnexpectedResponse resp

listBuckets :: Riak [L.ByteString]
listBuckets = do
  resp <- run M.listBuckets
  case resp of
    M.ListBuckets resp -> return $! F.toList $ buckets resp
    _ -> throw $ M.UnexpectedResponse resp

listKeys :: L.ByteString -> Riak [L.ByteString]
listKeys req = do
  resp <- run $ M.listKeys $ ListKeysRequest req
  case resp of
    M.ListKeys resps -> return $! concatMap (F.toList . LK.keys) resps
    _ -> throw $ M.UnexpectedResponse resp

getBucket :: L.ByteString -> Riak GetBucketResponse
getBucket req = do
  resp <- run $ M.getBucket $ GetBucketRequest req
  case resp of
    M.GetBucket resp -> return resp
    _ -> throw $ M.UnexpectedResponse resp

setBucket :: SetBucketRequest -> Riak ()
setBucket req = do
  resp <- run $ M.setBucket req
  case resp of
    M.SetBucket -> return ()
    _ -> throw $ M.UnexpectedResponse resp

mapReduce :: MR.MapReduceRequest -> Riak [MR.MapReduceResponse]
mapReduce req = do
  resp <- run $ M.mapReduce req
  case resp of
    M.MapReduce resps -> return resps
    _ -> throw $ M.UnexpectedResponse resp

indexQuery :: IndexRequest -> Riak [L.ByteString]
indexQuery req = do
  resp <- run $ M.index req
  case resp of
    M.Index resp -> return $! F.toList $ keys resp
    _ -> throw $ M.UnexpectedResponse resp

searchQuery :: SearchQueryRequest -> Riak SearchQueryResponse
searchQuery req = do
  resp <- run $ M.searchQuery req
  case resp of
    M.SearchQuery resp -> return resp
    _ -> throw $ M.UnexpectedResponse resp

runProto :: Connection -> O.RiakProto a -> IO a
runProto c p = runRiak c $ evalProto $ liftProgram $ O.unwrapRiakProgram p

evalProto :: ProgramT (O.RiakOp O.Proto) Riak a -> Riak a
evalProto = viewT >=> eval
  where
    eval :: ProgramViewT (O.RiakOp O.Proto) Riak a -> Riak a
    eval (O.GetOp r :>>= m) = get (toProtoRep r) >>= evalProto . m . fromProtoRep . (\x -> Tagged x :: Tagged b GR.GetResponse)
    eval (O.PutOp r :>>= m) = put (toProtoRep r) >>= evalProto . m . fromProtoRep . (\x -> Tagged x :: Tagged b PS.PutResponse)
    eval (O.DeleteOp r :>>= m) = delete (toProtoRep r) >>= evalProto . m
    eval (O.IndexOp b i q :>>= m) = indexQuery (mapIndexRequest b i q) >>= evalProto . m
    eval (O.ListBucketsOp :>>= m) = listBuckets >>= evalProto . m
    eval (O.GetClientIdOp :>>= m) = getClientId >>= evalProto . m
    eval (O.SetClientIdOp r :>>= m) = setClientId r >>= evalProto . m
    eval (O.ListKeysOp r :>>= m) = listKeys r >>= evalProto . m
    eval (O.PingOp :>>= m) = ping >>= evalProto . m . const True
    eval (Return a) = return a
