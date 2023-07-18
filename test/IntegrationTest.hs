{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (status500, statusCode)
import Network.Wai
  ( Application
  , rawPathInfo
  , responseLBS
  )
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Network.Wai.Middleware.Delegate
  ( ProxySettings (..)
  , delegateToProxy
  )
import System.Environment (lookupEnv)
import System.IO
import Test.Fetch (fetch)
import Test.Hspec
import Test.Hspec.TmpProc
  ( HList (..)
  , HostIpAddress
  , Pinged (..)
  , Proc (..)
  , ProcHandle
  , SvcURI
  , hAddr
  , handleOf
  , startupAll
  , tdescribe
  , terminateAll
  , toPinged
  , (&:)
  )
import qualified Test.Hspec.TmpProc as TmpProc
import Test.HttpReply
import Test.TestRequests
  ( RequestBuilder (..)
  , buildRequest
  , testNotProxiedRequests
  , testOverRedirectedRequests
  , testRequests
  )
import Test.WithExtras
  ( defaultTlsSettings
  , testWithTLSApplication
  )


defaultTestSettings :: ProxySettings
defaultTestSettings = def {proxyHost = "httpbin.org", proxyTimeout = 2}


redirectTestSettings :: ProxySettings
redirectTestSettings = defaultTestSettings {proxyRedirectCount = 2}


tmpHostSettings :: ByteString -> ProxySettings -> ProxySettings
tmpHostSettings tmpHost settings = settings {proxyHost = tmpHost}


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  dumpDebug' <- lookupEnv "DEBUG"
  let dumpDebug = maybe False (const True) dumpDebug'
  hspec $ tdescribe "when accessing http-bin in docker" $ do
    -- TODO: reenable when a secure proxy is added to the docker tests
    -- secureProxyTest dumpDebug
    -- secureNotProxiedTest dumpDebug
    insecureRedirectTest dumpDebug
    insecureNotProxiedTest dumpDebug
    insecureProxyTest dumpDebug


defaultTestDelegate :: ProxySettings -> IO Application
defaultTestDelegate s = do
  -- delegate everything but /status/418
  let handleFunnyStatus req = rawPathInfo req /= "/status/418"
      dummyApp _ respond = respond $ responseLBS status500 [] "I should have been proxied"

  manager <- newTlsManager
  return $ delegateToProxy s manager handleFunnyStatus dummyApp


toHost :: HttpBinFixture -> ByteString
toHost fixture = encodeUtf8 $ hAddr $ handleOf @"tmp-http-bin" Proxy fixture


fixtureApp :: HttpBinFixture -> IO Application
fixtureApp fixture = defaultTestDelegate $ tmpHostSettings (toHost fixture) defaultTestSettings


redirectApp :: HttpBinFixture -> IO Application
redirectApp fixture = defaultTestDelegate $ tmpHostSettings (toHost fixture) redirectTestSettings


testWithInsecureProxy' :: ((HttpBinFixture, Port) -> IO a) -> IO a
testWithInsecureProxy' = TmpProc.testWithApplication onlyHttpBin fixtureApp


testWithSecureProxy' :: ((HttpBinFixture, Port) -> IO a) -> IO a
testWithSecureProxy' action = do
  tls <- defaultTlsSettings
  TmpProc.testWithTLSApplication tls onlyHttpBin fixtureApp action


testWithInsecureProxy :: (Port -> IO ()) -> IO ()
testWithInsecureProxy = testWithApplication (defaultTestDelegate defaultTestSettings)


testWithSecureProxy :: (Port -> IO ()) -> IO ()
testWithSecureProxy withPort = do
  tls <- defaultTlsSettings
  testWithTLSApplication tls (defaultTestDelegate defaultTestSettings) withPort


testWithInsecureRedirects' :: ((HttpBinFixture, Port) -> IO ()) -> IO ()
testWithInsecureRedirects' = TmpProc.testWithApplication onlyHttpBin redirectApp


tmpHostBuilder :: HttpBinFixture -> RequestBuilder -> RequestBuilder
tmpHostBuilder fixture builder = builder {rbHost = toHost fixture}


onDirectAndProxy :: (HttpReply -> HttpReply -> IO ()) -> Bool -> Int -> RequestBuilder -> IO ()
onDirectAndProxy f debug testProxyPort builder = do
  let proxiedBuilder = builder {rbHost = "localhost", rbPort = Just testProxyPort}
  directReq <- buildRequest builder
  proxiedReq <- buildRequest proxiedBuilder

  when debug $ do
    putStrLn "---------------"
    putStrLn "Direct Request:"
    putStrLn "---------------"
    print directReq
    putStrLn "----------------"
    putStrLn "Proxied Request:"
    putStrLn "----------------"
    print proxiedReq
  proxied <- fetch proxiedReq
  direct <- fetch directReq
  when debug $ do
    putStrLn "Direct:"
    putStrLn "-------"
    print direct
    putStrLn "Proxied:"
    putStrLn "--------"
    print proxied
  f direct proxied


insecureNotProxiedTest :: Bool -> Spec
insecureNotProxiedTest debug =
  let scheme = "HTTP"
      desc = "Proxy on " ++ scheme ++ " should fail"
      assertNeq = onDirectAndProxy assertHttpRepliesDiffer debug
   in aroundAll testWithInsecureProxy' $ describe desc $ do
        for_ testNotProxiedRequests $ \(title, modifier) -> do
          let shouldNotMatch (f, p) = assertNeq p $ modifier $ tmpHostBuilder f def
          it (scheme ++ " " ++ title) shouldNotMatch


insecureRedirectTest :: Bool -> Spec
insecureRedirectTest debug =
  let scheme = "HTTP"
      desc = "Proxy over " ++ scheme ++ " with too many redirects differs"
      assertNeq = onDirectAndProxy assertHttpRepliesDiffer debug
   in aroundAll testWithInsecureRedirects' $ describe desc $ do
        for_ testOverRedirectedRequests $ \(title, modifier) -> do
          let shouldNotMatch (f, p) = assertNeq p $ modifier $ tmpHostBuilder f def
          it (scheme ++ " " ++ title) shouldNotMatch


insecureProxyTest :: Bool -> Spec
insecureProxyTest debug =
  let scheme = "HTTP"
      desc = "Simple " ++ scheme ++ " proxying:"
      assertEq = onDirectAndProxy assertHttpRepliesAreEq debug
   in aroundAll testWithInsecureProxy' $ describe desc $ do
        for_ testRequests $ \(title, modifier) -> do
          let shouldMatch (f, p) = assertEq p $ modifier $ tmpHostBuilder f def
          it (scheme ++ " " ++ title) shouldMatch


secureNotProxiedTest :: Bool -> Spec
secureNotProxiedTest debug =
  let scheme = "HTTPS"
      desc = "Proxy on " ++ scheme ++ " should fail"
      assertNeq = onDirectAndProxy assertHttpRepliesDiffer debug
      def' = def {rbSecure = True}
   in aroundAll testWithSecureProxy' $ describe desc $ do
        for_ testNotProxiedRequests $ \(title, modifier) -> do
          let shouldNotMatch (f, p) = assertNeq p $ modifier $ tmpHostBuilder f def'
          it (scheme ++ " " ++ title) shouldNotMatch


secureProxyTest :: Bool -> Spec
secureProxyTest debug =
  let scheme = "HTTPS"
      desc = "Simple " ++ scheme ++ " proxying:"
      assertEq = onDirectAndProxy assertHttpRepliesAreEq debug
      def' = def {rbSecure = True}
   in aroundAll testWithSecureProxy' $ describe desc $ do
        for_ testRequests $ \(title, modifier) -> do
          let shouldMatch (f, p) = assertEq p $ modifier $ tmpHostBuilder f def'
          it (scheme ++ " " ++ title) shouldMatch


-- | A data type representing a connection to a HttpBin server.
data HttpBin = HttpBin


type HttpBinFixture = HList '[ProcHandle HttpBin]


onlyHttpBin :: HList '[HttpBin]
onlyHttpBin = HttpBin &: HNil


setupHttpBin :: IO HttpBinFixture
setupHttpBin = startupAll onlyHttpBin


withHttpBin :: SpecWith HttpBinFixture -> Spec
withHttpBin = beforeAll setupHttpBin . afterAll terminateAll


-- | Run HttpBin using tmp-proc.
instance Proc HttpBin where
  type Image HttpBin = "kennethreitz/httpbin"
  type Name HttpBin = "tmp-http-bin"


  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


-- | Make a uri access the http-bin server.
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


ping' :: ProcHandle a -> IO Pinged
ping' handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- handleGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


-- | Determine the status from a Get on localhost.
handleGet :: ProcHandle a -> Text -> IO Int
handleGet handle urlPath = do
  let theUri = "http://" <> hAddr handle <> "/" <> Text.dropWhile (== '/') urlPath
  manager <- HC.newManager HC.defaultManagerSettings
  getReq <- HC.parseRequest $ Text.unpack theUri
  statusCode . HC.responseStatus <$> HC.httpLbs getReq manager
