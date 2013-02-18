{-# LANGUAGE OverloadedStrings #-}
module Zippy.Base.Client where
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Aeson hiding (Error)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntMap as H
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Zippy.Base.Common

keyToBS :: Key a -> ByteString
keyToBS = L.toStrict . fromKey

data Error = Error { errorStatus :: Status, message :: L.ByteString }
	deriving (Show, Eq)

data ClientConfig = ClientConfig
	{ baseRequest :: Request IO
	, manager :: Manager
	}

type Route = ByteString
type Handler a = (Int, Response L.ByteString -> Maybe a)
type ClientResult a = ReaderT ClientConfig (ResourceT IO) (Either Error a)

runClient :: String -> ClientResult a -> IO (Either Error a)
runClient r m = do
	base <- parseUrl r
	withManager $ \manager -> do
		runReaderT m (ClientConfig base manager)

nothingOn code = (code, const Nothing)
bodyOn code = (code, Just . jsonBody)

jsonBody :: FromJSON a => Response L.ByteString -> Maybe a
jsonBody = decode . responseBody

requestRoute :: (a -> L.ByteString) -> (Request IO -> Request IO) -> Route -> [Handler b] -> a -> ClientResult b
requestRoute fa fr route hs x = do
	let handlerMap = H.fromList hs
	config <- ask
	let req = (fr $ baseRequest config) { requestBody = RequestBodyLBS $ fa x, path = route, checkStatus = \_ _ -> Nothing }
	resp <- httpLbs req $ manager config
	let code = statusCode $ responseStatus resp
	return $! case H.lookup code handlerMap of
		Nothing -> Left $ Error (responseStatus resp) $ responseBody resp
		Just h -> case h resp of
			Nothing -> Left $ Error (responseStatus resp) ("Could not decode response: " <> responseBody resp)
			Just v -> Right v

-- | path, expected status code
get :: FromJSON a => Route -> [Handler a] -> ClientResult a
get r hs = requestRoute id id r hs ""

post :: (ToJSON a, FromJSON b) => Route -> [Handler b] -> a -> ClientResult b
post r hs x = requestRoute encode id r hs x

post' :: Route -> [Handler a] -> ClientResult a
post' r hs = requestRoute id (\req -> req { method = "POST" }) r hs ""

put :: (ToJSON a, FromJSON b) => Route -> [Handler b] -> a -> ClientResult b
put r hs x = requestRoute encode (\req -> req { method = "PUT" }) r hs x

delete :: (ToJSON a, FromJSON b) => Route -> [Handler b] -> a -> ClientResult b
delete r hs x = requestRoute encode (\req -> req { method = "DELETE" }) r hs x

delete' :: Route -> [Handler a] -> ClientResult a
delete' r hs = requestRoute id (\req -> req { method = "DELETE" }) r hs ""