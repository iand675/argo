{-# LANGUAGE OverloadedStrings #-}
module Zippy.Base.Data (
	Namespace(..),
	ByteString,
	module Data.Proxy,
	runData,
	key,
	redis,
	Key(..),
	Changeset(..),
	MultiDb,
	initialize,
	shutdown,
	DbConfig
) where
import Control.Monad.Reader
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString)
import Data.Proxy
import Database.Redis

data RiakConnection = RiakConnection
data RedisConnection = RedisConnection
data Pool a = Pool

class Namespace a where
	namespace :: Proxy a -> ByteString

data DbConfig = DbConfig
	{ riakPool :: Pool RiakConnection
	, redisConnection :: Connection
	, metricTracker :: ByteString -> IO ()
	}

type MultiDb = ReaderT DbConfig IO

initialize = connect defaultConnectInfo >>= \c -> return $ DbConfig undefined c undefined
shutdown c = runRedis (redisConnection c) quit

runData :: MultiDb a -> IO a
runData m = do
	conf <- initialize
	result <- runReaderT m conf
	runRedis (redisConnection conf) quit
	return result

key :: ByteString -> Key a
key = Key
--riak :: Riak a -> MultiDb a
--riak = undefined
redis :: Redis a -> MultiDb a
redis m = do
	conn <- fmap redisConnection ask
	liftIO $ runRedis conn m
--metric :: ByteString -> MultiDb ()
--metric = undefined
--uuid :: MultiDb ByteString
--uuid = undefined

newtype Key a = Key { fromKey :: ByteString }
newtype Changeset a = Changeset { fromChangeset :: Object }