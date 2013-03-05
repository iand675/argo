{-# LANGUAGE OverloadedStrings #-}
module Zippy.Base.Client where
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Aeson hiding (Error)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntMap as H
import Data.IORef
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Types.URI
import Zippy.Base.Common

keyToBS :: Key a -> ByteString
keyToBS = L.toStrict . fromKey

htmlKey :: Key a -> ByteString
htmlKey = urlEncode False . keyToBS

data Error = Error { errorStatus :: Status, message :: L.ByteString }
	deriving (Show, Eq)

data ClientConfig = ClientConfig
	{ baseRequest :: Request IO
	, manager :: Manager
	, cookieJarRef :: IORef CookieJar
	}

type Route = ByteString
type Handler a = (Int, Response L.ByteString -> Maybe a)
type ClientResult a = ReaderT ClientConfig (ResourceT IO) (Either Error a)

runClient :: String -> ClientResult a -> IO (Either Error a)
runClient r m = do
	cookieJarRef <- newIORef $ createCookieJar []
	base <- parseUrl r
	withManager $ \manager -> do
		runReaderT m (ClientConfig base manager cookieJarRef)

nothingOn :: Int -> Handler (Maybe a)
nothingOn code = (code, const $ Just Nothing)

bodyOn :: FromJSON a => Int -> Handler a
bodyOn code = (code, jsonBody)

jsonBody :: FromJSON a => Response L.ByteString -> Maybe a
jsonBody = decode . responseBody

getCookies :: ClientResult [Cookie]
getCookies = do
	config <- ask
	cookies <- liftIO $ readIORef $ cookieJarRef config
	return $ Right $ destroyCookieJar cookies

requestRoute :: (a -> L.ByteString) -> (Request IO -> Request IO) -> Route -> [Handler b] -> a -> ClientResult b
requestRoute fa fr route hs x = do
	let handlerMap = H.fromList hs
	config <- ask
	cookies <- liftIO $ readIORef $ cookieJarRef config
	timestamp <- liftIO getCurrentTime
	let (req, j) = insertCookiesIntoRequest ((fr $ baseRequest config) { requestBody = RequestBodyLBS $ fa x, path = route, checkStatus = \_ _ -> Nothing }) cookies timestamp
	liftIO $ writeIORef (cookieJarRef config) j
	resp <- httpLbs req $ manager config
	timestamp' <- liftIO getCurrentTime
	let (j', resp') = updateCookieJar resp req timestamp' j
	let code = statusCode $ responseStatus resp'
	liftIO $ writeIORef (cookieJarRef config) j'
	return $! case H.lookup code handlerMap of
		Nothing -> Left $ Error (responseStatus resp') $ responseBody resp'
		Just h -> case h resp' of
			Nothing -> Left $ Error (responseStatus resp') ("Could not decode response: " <> responseBody resp')
			Just v -> Right v

-- | path, expected status code
get :: FromJSON a => Route -> [Handler a] -> ClientResult a
get r hs = requestRoute id id r hs ""

post :: (ToJSON a, FromJSON b) => Route -> [Handler b] -> a -> ClientResult b
post r hs x = requestRoute encode (\req -> req { method = "POST" }) r hs x

post' :: Route -> [Handler a] -> ClientResult a
post' r hs = requestRoute id (\req -> req { method = "POST" }) r hs ""

put :: (ToJSON a, FromJSON b) => Route -> [Handler b] -> a -> ClientResult b
put r hs x = requestRoute encode (\req -> req { method = "PUT" }) r hs x

delete :: (ToJSON a, FromJSON b) => Route -> [Handler b] -> a -> ClientResult b
delete r hs x = requestRoute encode (\req -> req { method = "DELETE" }) r hs x

delete' :: Route -> [Handler a] -> ClientResult a
delete' r hs = requestRoute id (\req -> req { method = "DELETE" }) r hs ""