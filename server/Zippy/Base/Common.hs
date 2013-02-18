{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Zippy.Base.Common where
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding

toKey :: Text -> Key a
toKey = Key . encodeUtf8 . fromStrict

class Namespace a where
  namespace :: Proxy a -> ByteString

newtype Key a = Key { fromKey :: ByteString }
	deriving (ToJSON, FromJSON, Read, Show, Eq)

keyProxy :: Key a -> Proxy a
keyProxy = const Proxy

link :: Namespace a => ByteString -> Key a -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
link rel k = (Just $ namespace $ keyProxy k, Just $ fromKey k, Just rel)

data Entity a = Entity { key :: Key a, value :: a }