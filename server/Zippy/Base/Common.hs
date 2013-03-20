{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Zippy.Base.Common where
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Proxy
import Data.Tagged
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding

--class Migrate a b | a -> b where
--	migrate :: a -> Either String b
--	rollback :: b -> a

class DomainData a b | a -> b where
	toData :: a -> b

class DataDomain a b | a -> b where
	fromData :: a -> b

class DomainModel a b where
	toModel :: Proxy b -> a -> b

class ModelDomain a b | a -> b where
	fromModel :: a -> b

toKey :: Text -> Key a
toKey = Key . encodeUtf8 . fromStrict

class Namespace a where
  namespace :: Proxy a -> ByteString

newtype Key a = Key { fromKey :: ByteString }
	deriving (ToJSON, FromJSON, Read, Show, Eq)

keyProxy :: Key a -> Proxy a
keyProxy = const Proxy

class Rekey a b where
	rekey :: a -> b

instance Rekey (Key a) (Tagged b ByteString) where
	rekey = Tagged . fromKey

instance Rekey (Tagged a ByteString) (Key b) where
	rekey = Key . untag

instance Rekey (Key a) (Key b) where
	rekey = Key . fromKey

data Entity a = Entity { key :: Key a, etag :: ByteString, value :: a }
	deriving (Read, Show, Eq)

instance (ToJSON a) => ToJSON (Entity a) where
	toJSON e = Object $
		H.insert "_id" (toJSON $ fromKey $ key e) $
		H.insert "_tag" (toJSON $ etag e) o
		where (Object o) = toJSON (value e)

instance (FromJSON a) => FromJSON (Entity a) where
	parseJSON (Object v) = do
		k <- v .: "_id"
		e <- v .: "_tag"
		val <- parseJSON $ Object $ H.delete "_id" $ H.delete "_tag" $ v
		return $ Entity k e val
	parseJSON _ = mempty

instance Functor Entity where
	fmap f (Entity k e v) = Entity (rekey k) e $ f v