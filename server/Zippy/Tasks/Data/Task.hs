{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zippy.Tasks.Data.Task where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Proxy
import Data.Time
import Zippy.Base.Common
import Zippy.Base.Model
import qualified Zippy.Riak.Content as C
import qualified Zippy.Riak.Object as O
import Zippy.Riak.Simple
import Zippy.User.Data.User (User)

task :: Proxy Task
task = Proxy

instance Namespace Task where
	namespace = const "task"

data Task = Task
    { name         :: Text
    , description  :: Maybe Text
    , status       :: Text
    , createdAt    :: UTCTime
    , dueAt        :: Maybe UTCTime
    , priority     :: Maybe Text
    , tags         :: [Text]
    , archivedAt   :: Maybe UTCTime
    , timeEstimate :: Maybe Text
    , creator      :: Key User
    , assignedTo   :: Key User
    --, taskHistory  :: [TaskChangeset]
    --, taskRecurrences  :: Maybe RecurrenceRule
    } deriving (Read, Show, Eq)

deriveJSON id ''Task

instance O.AsContent Task where
	fromContent = decode . C.value
	value = encode
	contentType = const O.jsonContent

createTask :: Task -> O.Riak b (Maybe (Key Task))
createTask t = do
	pr <- putNew task () t
	return $! fmap Key $ C.key pr

getTask :: Key Task -> O.Riak b (Maybe Task)
getTask k = do
	gr <- get task () k
	case C.getContent gr of
		[] -> return Nothing
		(u:[]) -> return $ O.fromContent u
		_ -> return Nothing