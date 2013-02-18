{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, Rank2Types #-}
module Zippy.Base.Data (
  ByteString,
  module Data.Proxy,
  runData,
  redis,
  MultiDb,
  initialize,
  shutdown,
  DbConfig,
  DataRep(..)
) where
import Control.Monad.Reader
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString)
import Data.Proxy
import Database.Redis
import qualified Zippy.Riak.Object as O
import qualified Zippy.Riak.Connection as Riak
import qualified Zippy.Riak.ProtoBufImpl as Riak

class DataRep a d | a -> d where
  fromData :: d -> a

data RiakConnection = RiakConnection
data RedisConnection = RedisConnection
data Pool a = Pool

data DbConfig = DbConfig
                { riakConnection :: Riak.Connection
                , riakHandler :: forall a. Riak.Connection -> O.RiakProto a -> IO a
                , redisConnection :: Connection
                , metricTracker :: ByteString -> IO ()
                }

type MultiDb = ReaderT DbConfig IO

initialize = do
  redisConn <- connect defaultConnectInfo
  riakConn <- Riak.connect (Just "127.0.0.1") Nothing
  return $ DbConfig { riakConnection = riakConn
                    , riakHandler = Riak.runProto
                    , redisConnection = redisConn
                    , metricTracker = const (return ())
                    }

shutdown c = runRedis (redisConnection c) quit

runData :: MultiDb a -> IO a
runData m = do
  conf <- initialize
  result <- runReaderT m conf
  runRedis (redisConnection conf) quit
  Riak.disconnect (riakConnection conf)
  return result

riak :: O.RiakProto a -> MultiDb a
riak m = do
  conf <- ask
  liftIO $ (riakHandler conf) (riakConnection conf) m

redis :: Redis a -> MultiDb a
redis m = do
  conn <- fmap redisConnection ask
  liftIO $ runRedis conn m

--metric :: ByteString -> MultiDb ()
--metric = undefined
--uuid :: MultiDb ByteString
--uuid = undefined
