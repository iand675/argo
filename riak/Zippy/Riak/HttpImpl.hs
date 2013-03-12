{-##-}
module Zippy.Riak.HttpImpl where
import Control.Monad.Operational
import Network.HTTP.Conduit
import Zippy.Riak.Object

data RouteLayout m = RouteLayout
    { bucketRoute              :: Request m
    , secondaryIndexRoute      :: Request m
    , keyListRoute             :: Request m
    , linkWalkRoute            :: Request m
    , mapReduceRoute           :: Request m
    , objectRoute              :: Request m
    , pingRoute                :: Request m
    , setBucketPropertiesRoute :: Request m
    , statsRoute               :: Request m
    , searchRoute              :: Request m
    , indexingRoute            :: Request m
    }

initializeHttp :: Manager -> String -> IO RouteLayout
initializeHttp = 

runRiak :: Manager -> String -> Riak HTTP a -> IO a
runRiak manager baseUrl actionProg = viewT (liftProgram actionProg) >>= eval
    where
    eval (GetOp b k r :>>= m)    = httpGet manager baseUrl b k r >>= m
    eval (PutOp b mk r :>>= m)   = httpPut manager baseUrl b mk r >>= m
    eval (DeleteOp b k r :>>= m) = httpDelete manager baseUrl b k r >>= m
    eval (IndexOp b i q :>>= m)  = httpIndex manager baseUrl manager baseUrl b i q >>= m
    eval (ListBucketsOp :>>= m)  = httpListBuckets manager baseUrl >>= m
    eval (ListKeysOp :>>= m)     = httpListKeys manager baseUrl >>= m
    eval (PingOp :>>= m)         = httpPing manager baseUrl >>= m
    eval (Return a)              = return a
    eval _ = error "Operation not supported using HTTP Riak API"

httpGet :: Manager -> String -> Bucket -> Key -> C.GetRequest -> IO (C.GetResponse a)
httpGet = 

httpPut :: Manager -> String -> Bucket -> Maybe Key -> C.PutRequest -> IO (C.PutResponse a)
httpDelete :: Manager -> String -> Bucket -> Key -> C.DeleteRequest -> IO ()
httpIndex :: Manager -> String -> Bucket -> Index -> IndexQuery -> IO [Key]
httpListBuckets :: Manager -> String -> IO [Bucket]
httpListKeys :: Manager -> String -> Bucket -> IO [Key]
httpPing :: Manager -> String -> IO Bool
