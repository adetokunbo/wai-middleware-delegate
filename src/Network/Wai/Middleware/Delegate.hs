{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Wai.Middleware.Delegate
Description :
  Provides a Wai middleware1
that delegates handling of requests.

  - delegateTo: delegates handling of requests matching a predicate to a
    delegate Application

  - delegateToProxy : delegates handling of requests matching a predicate to
    different host

  - simpleProxy: is a simple reverse proxy, based on proxyApp of http-proxy by Erik
    de Castro Lopo/Michael Snoyman

Copyright   : (c) Tim Emiola, 2018
License     : C8D3
Maintainer  : tim.emiola@gmail.com
Stability   : experimental
-}

module Network.Wai.Middleware.Delegate
  ( delegateTo
  , delegateToProxy
  , simpleProxy
  , ProxySettings(..)
  , RequestPredicate
  )

where

import           Control.Exception           (SomeException, handle,
                                              toException)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy.Char8  as LC8
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
import           Network.HTTP.Types.Header   (hHost)
import qualified Network.Wai                 as Wai
import           Network.Wai.Conduit         (responseRawSource, responseSource,
                                              sourceRequestBody)

-- | Type alias for a function that determines if a request should be handled by
-- a delegate.
type RequestPredicate = Wai.Request -> Bool

-- | Create a middleware that handles all requests matching a predicate by
-- delegating to an alternate Application.
delegateTo :: Wai.Application -> RequestPredicate -> Wai.Middleware
delegateTo alt f actual req
  | f req = alt req
  | otherwise = actual req

-- | Creates a middleware that handles all requests matching a predicate by
-- proxing them to a host specified by ProxySettings.
delegateToProxy :: ProxySettings -> Manager -> RequestPredicate -> Wai.Middleware
delegateToProxy settings mgr = delegateTo (simpleProxy settings mgr)

-- | Settings that configure the proxy endpoint.
data ProxySettings =
  ProxySettings
  { -- | What to do with exceptions thrown by either the application or server.
    proxyOnException :: SomeException -> Wai.Response
    -- | Timeout value in seconds. Default value: 30
  , proxyTimeout     :: Int
    -- | The host being proxied
  , proxyHost        :: BS.ByteString
    -- | The number of redirects to follow. 0 means none, which is the default.
  , proxyRedirectCount    :: Int
  }

instance Default ProxySettings where
  -- | The default settings for the Proxy server. See the individual settings for
  -- the default value.
  def = ProxySettings
    { -- defaults to returning internal server error showing the error in the body
      proxyOnException = onException
      -- default to 15 seconds
    , proxyTimeout = 15
    , proxyHost = "localhost"
    , proxyRedirectCount = 0
    }
    where
      onException :: SomeException -> Wai.Response
      onException e =
        Wai.responseLBS internalServerError500
        [ (hContentType, "text/plain; charset=utf-8") ] $
        LC8.fromChunks [C8.pack $ show e]

-- | A Wai Application that acts as a http/https proxy.
simpleProxy
  :: ProxySettings
  -> Manager
  -> Wai.Application
simpleProxy settings manager req respond
    -- we may connect requests to secure sites, when we do, we will not have
    -- seen their URI properly
    | Wai.requestMethod req == "CONNECT" = do
        putStrLn $ "Seen a CONNECT !!! to path " ++ (C8.unpack $ Wai.rawPathInfo req)
        respond $ responseRawSource (handleConnect req)
                    (Wai.responseLBS status500 [("Content-Type", "text/plain")] "method CONNECT is not supported")
    | otherwise = do
        let scheme
              | Wai.isSecure req = "https"
              | otherwise = "http"
            rawUrl = Wai.rawPathInfo req <> Wai.rawQueryString req
            effectiveUrl = scheme ++ "://" ++ (C8.unpack $ proxyHost settings) ++ C8.unpack (rawUrl)
            newHost = proxyHost settings
            addHostHeader = (:) (hHost, newHost)

        proxyReq' <- parseRequest effectiveUrl
        let onException :: SomeException -> Wai.Response
            onException = proxyOnException settings . toException

            proxyReq = proxyReq'
              { method = Wai.requestMethod req
              , requestHeaders = addHostHeader $ filter dropUpstreamHeaders $ Wai.requestHeaders req
                -- always pass redirects back to the client.
              , redirectCount = proxyRedirectCount settings
              , requestBody =
                  case Wai.requestBodyLength req of
                    Wai.ChunkedBody ->
                      requestBodySourceChunkedIO (sourceRequestBody req)
                    Wai.KnownLength l ->
                      requestBodySourceIO (fromIntegral l) (sourceRequestBody req)
              -- don't modify the response to ensure consistency with the response headers
              , decompress = const False
              , host = newHost
              }

            respondUpstream = withResponse proxyReq manager $ \res -> do
              let body = mapOutput (Chunk . fromByteString) . bodyReaderSource $ responseBody res
                  headers = (mk "X-Via-Proxy", "yes") : (responseHeaders res)
              respond $ responseSource (responseStatus res) headers body

        handle (respond . onException) respondUpstream

handleConnect
  :: Wai.Request
  -> ConduitT () C8.ByteString IO ()
  -> ConduitT C8.ByteString Void IO ()
  -> IO ()
handleConnect req fromClient toClient =
  runTCPClient (toClientSettings req) $ \ad -> do
  runConduit $ yield "HTTP/1.1 200 OK\r\n\r\n" .| toClient
  race_
    (runConduit $ fromClient .| appSink ad)
    (runConduit $ appSource ad .| toClient)

defaultClientPort :: Wai.Request -> Int
defaultClientPort req
  | Wai.isSecure req = 443
  | otherwise = 90

toClientSettings :: Wai.Request -> ClientSettings
toClientSettings req =
  case C8.break (== ':') $ Wai.rawPathInfo req of
    (host, "") -> clientSettingsTCP (defaultClientPort req) host
    (host, port') -> case C8.readInt $ C8.drop 1 port' of
      Just (port, _) -> clientSettingsTCP port host
      Nothing        -> clientSettingsTCP (defaultClientPort req) host

dropUpstreamHeaders :: (Eq a, IsString a) => (a, b) -> Bool
dropUpstreamHeaders (k, _) = k `notElem`
  [ "content-encoding"
  , "content-length"
  , "host"
  ]
