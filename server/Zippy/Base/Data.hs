{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, Rank2Types, GeneralizedNewtypeDeriving #-}
module Zippy.Base.Data (
  module Data.Proxy,
  runData,
  riak,
  redis,
  MultiDb,
  initialize,
  shutdown,
  DbConfig,
  DataError(..),
  ifNothing,
  justOne,
  link,
  binIndex,
  keyIndex,
  success,
  failure,
  convertEither,
  onFailure
) where
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid
import Data.Proxy
import Database.Redis
import Zippy.Base.Common
import qualified Zippy.Riak.Object as O
import qualified Zippy.Riak.Connection as Riak
import qualified Zippy.Riak.Content as Riak
import qualified Zippy.Riak.ProtoBufImpl as Riak

success :: a -> MultiDb a
success = MultiDb . right

failure :: DataError -> MultiDb a
failure = MultiDb . left

onFailure :: (DataError -> ReaderT DbConfig IO b) -> (a -> ReaderT DbConfig IO b) -> MultiDb a -> MultiDb b
onFailure fl fr m = MultiDb $ lift $ eitherT fl fr (fromMultiDb m)

convertEither :: Either DataError a -> MultiDb a
convertEither = MultiDb . hoistEither

link :: Namespace a => ByteString -> Key a -> Riak.Link
link rel k = Riak.Link (namespace $ keyProxy k) (fromKey k) $ Just rel

binIndex :: ByteString -> ByteString -> Riak.Index
binIndex i v = Riak.Index (i <> "_bin") v

keyIndex :: ByteString -> Key a -> Riak.Index
keyIndex i k = binIndex i $ fromKey k

data DataError = NotFound
               | AlreadyExists
               | DeserializationError
               | DataConflict
               deriving (Read, Show, Eq)

ifNothing :: DataError -> Maybe a -> MultiDb a
ifNothing l mr = case mr of
  Nothing -> failure l
  Just r -> success r

justOne :: [a] -> MultiDb a
justOne [] = failure NotFound
justOne (x:[]) = success x
justOne _ = failure DataConflict

data RiakConnection = RiakConnection
data RedisConnection = RedisConnection
data Pool a = Pool

data DbConfig = DbConfig
                { riakConnection :: Riak.Connection
                , riakHandler :: forall a. Riak.Connection -> O.RiakProto a -> IO a
                , redisConnection :: Connection
                , metricTracker :: ByteString -> IO ()
                }

newtype MultiDb a = MultiDb { fromMultiDb :: EitherT DataError (ReaderT DbConfig IO) a }
  deriving (Monad, Functor)

initialize = do
  redisConn <- connect defaultConnectInfo
  riakConn <- Riak.connect (Just "10.0.2.15") Nothing
  return $ DbConfig { riakConnection = riakConn
                    , riakHandler = Riak.runProto
                    , redisConnection = redisConn
                    , metricTracker = const (return ())
                    }

shutdown c = runRedis (redisConnection c) quit

runData :: MonadIO m => MultiDb a -> m (Either DataError a)
runData m = liftIO $ do
  conf <- initialize
  result <- runReaderT (runEitherT $ fromMultiDb m) conf
  runRedis (redisConnection conf) quit
  Riak.disconnect (riakConnection conf)
  return result

riak :: O.RiakProto (Either DataError a) -> MultiDb a
riak m = MultiDb $ do
  conf <- ask
  r <- liftIO $ (riakHandler conf) (riakConnection conf) m
  hoistEither r

redis :: Redis (Either DataError a) -> MultiDb a
redis m = MultiDb $ do
  conn <- fmap redisConnection ask
  r <- liftIO $ runRedis conn m
  hoistEither r

--metric :: ByteString -> MultiDb ()
--metric = undefined
--uuid :: MultiDb ByteString
--uuid = undefined
