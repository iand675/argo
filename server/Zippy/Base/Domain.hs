module Zippy.Base.Domain where
import Data.ByteString (ByteString)

newtype Key a = Key { fromKey :: ByteString }
data Entity a = Entity { key :: Key a, value :: a }