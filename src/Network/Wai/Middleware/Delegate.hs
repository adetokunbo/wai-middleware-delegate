{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Wai.Middleware.Delegate
Description :
  Provides Wai middleware that delegate to handling of requests

  - delegateTo: delegates handling of requests matching a predicate to a
    delegate Application

  - delegateToProxy : delegates handling of requests matching a predicate to
    different host

  - devProxy: is a simple reverse proxy, based on proxyApp of http-proxy by Erik
    de Castro Lopo/Michael Snoyman

Copyright   : (c) Tim Emiola, 2018
License     : BSD3
Maintainer  : tim.emiola@gmail.com
Stability   : experimental
-}

module Network.Wai.Middleware.Delegate
  ( delegateTo
  , delegateToProxy
  , devProxy
  , ProxySettings(..)
  , RequestPredicate
  )

where

import           Control.Exception           (SomeException, handle,
                                              toException)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Monoid                 ((<>))
import           Data.String                 (IsString)

import           Blaze.ByteString.Builder    (fromByteString)
import           Control.Concurrent.Async    (race_)
import           Data.CaseInsensitive        (mk)
import           Data.Conduit                (ConduitT, Flush (..), Void,
                                              mapOutput, runConduit, yield,
                                              (.|))
import           Data.Conduit.Network        (appSink, appSource)
import           Data.Default                (Default (..))
import           Data.Streaming.Network      (ClientSettings, clientSettingsTCP,
                                              runTCPClient)
import           Network.HTTP.Client         (Manager, Request (..),
                                              Response (..), parseRequest,
                                              withResponse)
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Conduit        (requestBodySourceChunkedIO,
                                              requestBodySourceIO)
import           Network.HTTP.Types          (hContentType,
                                              internalServerError500, status304,
                                              status500)
import qualified Network.Wai                 as Wai
import           Network.Wai.Conduit         (responseRawSource, responseSource,
                                              sourceRequestBody)

-- Determines if a request should be handled by a delegate.
type RequestPredicate = Wai.Request -> Bool

-- Create a middleware that handles all requests matching a predicate by
-- delegating to an alternate Application.
delegateTo :: Wai.Application -> RequestPredicate -> Wai.Middleware
delegateTo alt f actual req
  | f req = alt req
  | otherwise = actual req

-- Creates a middleware that handles all requests matching a predicate by
-- proxing them to a host specified by ProxySettings.
delegateToProxy :: ProxySettings -> Manager -> RequestPredicate -> Wai.Middleware
delegateToProxy settings mgr = delegateTo (devProxy settings mgr)

-- | Settings that configure the proxy endpoint.
data ProxySettings =
  ProxySettings
  { -- ^ What to do with exceptions thrown by either the application or server.
    proxyOnException :: SomeException -> Wai.Response
    -- ^ Timeout value in seconds. Default value: 30
  , proxyTimeout     :: Int
  }

instance Default ProxySettings where
  -- | The default settings for the Proxy server. See the individual settings for
  -- the default value.
  def = ProxySettings
    { -- defaults to returning internal server error showing the error in the body
      proxyOnException = onException
      -- default to 15 seconds
    , proxyTimeout = 15
    }
    where
      onException :: SomeException -> Wai.Response
      onException e =
        Wai.responseLBS internalServerError500
        [ (hContentType, "text/plain; charset=utf-8") ] $
        LBS.fromChunks [BS.pack $ show e]

-- | A Wai Application that acts as a http/https proxy.
devProxy
  :: ProxySettings
  -> Manager
  -> Wai.Application
devProxy settings manager req respond
    | Wai.requestMethod req == "CONNECT" =
        respond $ responseRawSource (handleConnect req)
                    (Wai.responseLBS status500 [("Content-Type", "text/plain")] "method CONNECT is not supported")
    | otherwise = do
        proxyReq' <- parseRequest $ BS.unpack (Wai.rawPathInfo req <> Wai.rawQueryString req)
        let onException :: SomeException -> Wai.Response
            onException = proxyOnException settings . toException

            proxyReq = proxyReq'
              { method = Wai.requestMethod req
              , requestHeaders = filter dropRequestHeader $ Wai.requestHeaders req
                -- always pass redirects back to the client.
              , redirectCount = 0
              , requestBody =
                  case Wai.requestBodyLength req of
                    Wai.ChunkedBody ->
                      requestBodySourceChunkedIO (sourceRequestBody req)
                    Wai.KnownLength l ->
                      requestBodySourceIO (fromIntegral l) (sourceRequestBody req)
              -- don't modify the response to ensure consistency with the response headers
              , decompress = const False
              }

            respondUpstream = withResponse proxyReq manager $ \res -> do
              let body = mapOutput (Chunk . fromByteString) . bodyReaderSource $ responseBody res
                  headers = (mk "X-Via-Proxy", "yes") : (responseHeaders res)
              respond $ responseSource (responseStatus res) headers body

        handle (respond . onException) respondUpstream

handleConnect
  :: Wai.Request
  -> ConduitT () BS.ByteString IO ()
  -> ConduitT BS.ByteString Void IO ()
  -> IO ()
handleConnect req fromClient toClient =
  runTCPClient (toClientSettings req) $ \ad -> do
  runConduit $ yield "HTTP/1.1 200 OK\r\n\r\n" .| toClient
  race_
    (runConduit $ fromClient .| appSink ad)
    (runConduit $ appSource ad .| toClient)

defaultClientPort :: Int
defaultClientPort = 80

toClientSettings :: Wai.Request -> ClientSettings
toClientSettings req =
  case BS.break (== ':') $ Wai.rawPathInfo req of
    (host, "") -> clientSettingsTCP defaultClientPort host
    (host, port') -> case BS.readInt $ BS.drop 1 port' of
      Just (port, _) -> clientSettingsTCP port host
      Nothing        -> clientSettingsTCP defaultClientPort host

dropRequestHeader :: (Eq a, IsString a) => (a, b) -> Bool
dropRequestHeader (k, _) = k `notElem`
  [ "content-encoding"
  , "content-length"
  ]
