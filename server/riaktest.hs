import Zippy.Riak.Connection
import Data.Time.Clock
import qualified Zippy.Riak.ProtoBufImpl as P
import Zippy.Base.Common
import Zippy.Riak.Simple
import Zippy.User.Data.User

--mahBuckets = do
  --conn <- connect (Just "127.0.0.1") Nothing
  --ks <- P.runProto conn $ listKeys "bucket"
  --disconnect conn
  --return ks

mahBuckets2 n = do
	time <- getCurrentTime
	conn <- connect (Just "127.0.0.1") Nothing
	ks <- P.runProto conn $ createUser (User n "Ian Duncan" Nothing "ian@iankduncan.com" "garbage" Nothing Nothing time)
	disconnect conn
	return ks

action m = do
	conn <- connect (Just "127.0.0.1") Nothing
	x <- P.runProto conn m
	disconnect conn
	return x