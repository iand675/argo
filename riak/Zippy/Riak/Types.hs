module Zippy.Riak.Types where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Builder as LB
import Data.Proxy
import Data.Tagged

type Key a = Tagged a L.ByteString

type Bucket a = Tagged a L.ByteString

newtype Request = Request { fromRequest :: LB.Builder }

newtype VClock = VClock { fromVClock :: L.ByteString }

type ClientId = L.ByteString

type IndexName a = L.ByteString

class Namespace a where
	namespace :: Proxy a -> L.ByteString