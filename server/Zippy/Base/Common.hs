{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings #-}
module Zippy.Base.Common where
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as H
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

index :: ByteString -> ByteString -> (ByteString, Maybe ByteString)
index i v = (i, Just v)

keyIndex :: ByteString -> Key a -> (ByteString, Maybe ByteString)
keyIndex i k = index i $ fromKey k

rekey :: Key a -> Key b
rekey = Key . fromKey

data Entity a = Entity { key :: Key a, value :: a }

instance (ToJSON a) => ToJSON (Entity a) where
	toJSON e = Object $ H.insert "_id" (toJSON $ fromKey $ key e) o
		where (Object o) = toJSON (value e)

instance Functor Entity where
	fmap f (Entity k v) = Entity (rekey k) $ f v