{-# LANGUAGE TypeFamilies #-}
module Zippy.Riak.Connection (
  ConnectionSettings(..),
  riakPool,
  RiakConnectionPool,
  RiakConnection(..),
  Connection,
  connect,
  disconnect,
  runRequest,
  decodeRecv
) where
import qualified Network.Socket as S
import Data.Attoparsec (Result)
import Data.IORef
import Data.Maybe
import Data.Pool
import Data.Time.Clock
import Zippy.Riak.Types
import Zippy.Riak.Messages (getResponse, handleRemaining, Response)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Builder
import Network.Socket.ByteString

data ConnectionSettings = ConnectionSettings
  { host               :: Maybe String
  , port               :: Maybe Int
  , connectionTimeout  :: Maybe NominalDiffTime
  , connectionsPerPool :: Maybe Int
  , pools              :: Maybe Int
  }

riakPool :: ConnectionSettings -> IO RiakConnectionPool
riakPool s = fmap RiakConnectionPool $ createPool (connect (host s) $ port s) disconnect
  (fromMaybe 3 $ pools s)
  (fromMaybe 30 $ connectionTimeout s)
  (fromMaybe 10 $ connectionsPerPool s)

newtype RiakConnectionPool = RiakConnectionPool
  { fromRiakConnectionPool :: Pool Connection
  }

class RiakConnection c where
  withConnection :: c -> (Connection -> IO a) -> IO a

data Connection = Connection
  { connSocket :: S.Socket
  , connLeftovers :: IORef B.ByteString
  }

connect :: Maybe S.HostName -> Maybe Int -> IO Connection
connect h p = do
  let hints = S.defaultHints { S.addrFlags = [S.AI_ADDRCONFIG], S.addrSocketType = S.Stream }
  ais <- S.getAddrInfo (Just hints) h (Just $ maybe "8087" show p)
  let ai = head ais
  s <- S.socket (S.addrFamily ai) (S.addrSocketType ai) (S.addrProtocol ai)
  S.setSocketOption s S.NoDelay 1
  S.connect s $ S.addrAddress ai
  left <- newIORef B.empty
  return $! Connection s left

disconnect :: Connection -> IO ()
disconnect = S.close . connSocket

instance RiakConnection Connection where
  withConnection c f = f c

instance RiakConnection RiakConnectionPool where
  withConnection p = withResource (fromRiakConnectionPool p)

runRequest :: Request -> Connection -> IO Response
runRequest b c = do
  sendRequest c b
  decodeRecv c

decodeRecv :: Connection -> IO Response
decodeRecv c = decodeRecv' c getResponse

decodeRecv' :: Connection -> (B.ByteString -> Result Response) -> IO Response
decodeRecv' c f = do
  bs <- recv (connSocket c) window
  let response = handleRemaining bs f
  case response of
    Left f' -> decodeRecv' c f'
    Right (value, remainder) -> do
      writeIORef (connLeftovers c) remainder
      return $! value
  where
    window = 16384

sendRequest :: Connection -> Request -> IO ()
sendRequest c r = sendMany (connSocket c) $ L.toChunks $ toLazyByteString $ fromRequest r
