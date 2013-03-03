{-# LANGUAGE OverloadedStrings #-}
module Zippy.Base.Web where
import Control.Monad.Reader
import Control.Monad.Trans
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Web.Cookie
import qualified Web.Scotty as S
import Zippy.Base.Data (DataError(..), runData, MultiDb(..))

hSetCookie = "Set-Cookie"

cookies :: Handler c [(Text, Text)]
cookies = do
	req <- request
	return $! case lookup hCookie $ requestHeaders $ req of
		Nothing -> []
		Just bs -> parseCookiesText bs

expireCookie :: [(Text, Text)] -> [(Text, Text)]
expireCookie = (("Expires", decodeUtf8 $ formatCookieExpires (UTCTime (ModifiedJulianDay 0) 0)) :) . filter ((/= "expires") . fst)

-- a function that looks up the user id from a session store
-- given the cookie and returns that result
-- if lookup fails, invalidate cookie
authSource :: (Text -> IO (Maybe Text)) -> Handler c (Maybe Text)
authSource f = do
	cs <- cookies
	let authCookie = lookup "token" cs
	case authCookie of
		Nothing -> return Nothing
		Just c -> do
			mk <- liftIO $ f c
			case mk of
				Nothing -> do
					--header hSetCookie $ expireCookie cs
					return Nothing
				k -> return k


handle :: c -> Handler c a -> S.ActionM a
handle conf m = runReaderT m conf

config :: Handler c c
config = ask

request :: Handler c Request
request = lift S.request

reqHeader :: L.Text -> Handler c L.Text
reqHeader = lift . S.reqHeader

body :: Handler c ByteString
body = lift S.body

param :: S.Parsable a => L.Text -> Handler c a
param = lift . S.param

params :: Handler c [S.Param]
params = lift S.params

jsonData :: FromJSON a => Handler c a
jsonData = lift S.jsonData

files :: Handler c [S.File]
files = lift S.files

status :: Status -> Handler c ()
status = lift . S.status

header :: L.Text -> L.Text -> Handler c ()
header k v = lift $ S.header k v

redirect :: L.Text -> Handler c a
redirect = lift . S.redirect

text :: L.Text -> Handler c ()
text = lift . S.text

html :: L.Text -> Handler c ()
html = lift . S.html

file :: FilePath -> Handler c ()
file = lift . S.file

json :: ToJSON a => a -> Handler c ()
json = lift . S.json

-- source = lift . S.source

raise :: L.Text -> Handler c a
raise = lift . S.raise

-- rescue :: Handler c a -> ()
-- rescue m f = lift $ S.rescue m f

next :: Handler c a
next = lift S.next

now :: Handler c UTCTime
now = liftIO getCurrentTime

type Handler c = ReaderT c S.ActionM

instance S.Parsable ByteString where
	parseParam = Right . L.encodeUtf8

instance S.Parsable Text where
	parseParam = Right . L.toStrict

handleDataError :: DataError -> Handler c ()
handleDataError e = case e of
	NotFound -> status notFound404
	AlreadyExists -> status badRequest400
	DeserializationError -> status internalServerError500
	DataConflict -> status conflict409

withData :: MultiDb a -> (a -> Handler c ()) -> Handler c ()
withData m f = do
	mresult <- runData m
	case mresult of
		Left err -> handleDataError err
		Right r -> f r
