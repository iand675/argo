{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies #-}
module Zippy.Base.Riak.Operations where
import Control.Monad.Operational
import Control.Monad.Reader
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.IORef

type Riak = ReaderT RiakConfig IO
type VClock = ByteString

data GetConfig = GetConfig
	--{ 
	--,
	--}

data PutConfig = PutConfig
	--{
	--,
	--}

data DeleteConfig = DeleteConfig
	--{
	--,
	--}

data RiakConfig = RiakConfig
	{ getConfig :: GetConfig
	, putConfig :: PutConfig
	, deleteConfig :: DeleteConfig
	--, vclock :: IORef (Maybe VClock)
	}

class HasVClock a where
	vclock :: a -> VClock

getSettings :: (GetConfig -> GetConfig) -> Riak a -> Riak a
getSettings f m = local (\c -> c { getConfig = f $ getConfig c }) m

putSettings :: (PutConfig -> PutConfig) -> Riak a -> Riak a
putSettings f m = local (\c -> c { putConfig = f $ putConfig c }) m

deleteSettings :: (DeleteConfig -> DeleteConfig) -> Riak a -> Riak a
deleteSettings f m = local (\c -> c { deleteConfig = f $ deleteConfig c }) m

data Link = Link
	{ bucket :: Maybe ByteString
	, key :: Maybe ByteString
	, tag :: Maybe ByteString
	}

data Content = Content
	{ vtag :: Maybe ByteString
	, lastModified :: Maybe Int
	, lastModifiedUSecs :: Maybe Int
	, contentType :: Maybe ByteString
	, charset :: Maybe ByteString
	, links :: [Link]
	, userMetadata :: [(ByteString, ByteString)]
	, indexes :: [(ByteString, ByteString)]
	, value :: ByteString
	}

class RiakObject a where {}
	-- decode :: Content ByteString -> Content (Either String r)

data RiakOp a where
	Get :: ByteString -> ByteString -> RiakOp (Maybe [Content])
	Put :: ByteString -> Maybe ByteString -> Content -> RiakOp (Maybe [Content])
	Delete :: ByteString -> ByteString -> RiakOp ()

--updateVClock :: Maybe VClock -> Riak ()
--updateVClock vc = do
--	conf <- ask
--	liftIO $ modifyIORef' (vclock conf) (const vc)

get :: ByteString -> ByteString -> ProgramT RiakOp m (Maybe [Content])
get k v = singleton $ Get k v

put :: ByteString -> Maybe ByteString -> Content -> ProgramT RiakOp m (Maybe [Content])
put k mv c = singleton $ Put k mv c

delete :: ByteString -> ByteString -> ProgramT RiakOp m ()
delete k v = singleton $ Delete k v

--get :: 
--put
--delete
--mapReduce
--search
--index