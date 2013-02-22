{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, Rank2Types #-}
module Zippy.Base.Data (
  module Data.Proxy,
  runData,
  riak,
  redis,
  MultiDb,
  initialize,
  shutdown,
  DbConfig,
  DataRep(..),
  DataError(..),
  ifNothing,
  justOne
) where
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString)
import Data.Proxy
import Database.Redis
import qualified Zippy.Riak.Object as O
import qualified Zippy.Riak.Connection as Riak
import qualified Zippy.Riak.ProtoBufImpl as Riak

data DataError = NotFound
               | AlreadyExists
               | DeserializationError
               | DataConflict
               deriving (Read, Show, Eq)

class DataRep a d | a -> d where
  fromData :: d -> a

ifNothing :: b -> Maybe a -> Either b a
ifNothing l mr = case mr of
  Nothing -> Left l
  Just r -> Right r

justOne :: [a] -> Either DataError a
justOne [] = Left NotFound
justOne (x:[]) = Right x
justOne _ = Left DataConflict

data RiakConnection = RiakConnection
data RedisConnection = RedisConnection
data Pool a = Pool

data DbConfig = DbConfig
                { riakConnection :: Riak.Connection
                , riakHandler :: forall a. Riak.Connection -> O.RiakProto a -> IO a
                , redisConnection :: Connection
                , metricTracker :: ByteString -> IO ()
                }

type MultiDb a = ReaderT DbConfig IO (Either DataError a)

initialize = do
  redisConn <- connect defaultConnectInfo
  riakConn <- Riak.connect (Just "127.0.0.1") Nothing
  return $ DbConfig { riakConnection = riakConn
                    , riakHandler = Riak.runProto
                    , redisConnection = redisConn
                    , metricTracker = const (return ())
                    }

shutdown c = runRedis (redisConnection c) quit

runData :: MonadIO m => MultiDb a -> m (Either DataError a)
runData m = liftIO $ do
  conf <- initialize
  result <- runReaderT m conf
  runRedis (redisConnection conf) quit
  Riak.disconnect (riakConnection conf)
  return result

riak :: O.RiakProto (Either DataError a) -> MultiDb a
riak m = do
  conf <- ask
  liftIO $ (riakHandler conf) (riakConnection conf) m

redis :: Redis (Either DataError a) -> MultiDb a
redis m = do
  conn <- fmap redisConnection ask
  liftIO $ runRedis conn m

--metric :: ByteString -> MultiDb ()
--metric = undefined
--uuid :: MultiDb ByteString
--uuid = undefined
