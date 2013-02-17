module Zippy.Riak.Types where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Builder as LB

newtype Request = Request { fromRequest :: LB.Builder }

newtype VClock = VClock { fromVClock :: L.ByteString }

type Key = L.ByteString

type ClientId = L.ByteString

type IndexName a = L.ByteString
