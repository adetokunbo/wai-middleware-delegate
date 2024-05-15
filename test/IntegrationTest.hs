{-# LANGUAGE DataKinds #-}
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
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (status500, statusCode)
import Network.Wai
  ( Application
  , rawPathInfo
  , responseLBS
  )
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Network.Wai.Handler.WarpTLS (tlsSettings)
import Network.Wai.Middleware.Delegate
  ( ProxySettings (..)
  , delegateToProxy
  )
import System.Environment (lookupEnv)
import System.IO
import Test.Certs.Temp (certificatePath, keyPath, withCertPathsInTmp')
import Test.Fetch (fetch)
import Test.Hspec
import Test.Hspec.TmpProc
  ( HList (..)
  , HandlesOf
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
  , (&:&)
  )
import qualified Test.Hspec.TmpProc as TmpProc
import Test.HttpReply
import Test.NginxGateway (NginxGateway (..), mkBadTlsManager)
import Test.TestRequests
  ( RequestBuilder (..)
  , buildRequest
  , nil
  , secure
  , testNotProxiedRequests
  , testOverRedirectedRequests
  , testRequests
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
  dumpDebug <- isJust <$> lookupEnv "DEBUG"
  hspec $ tdescribe "accessing http-bin in docker" $ do
    insecureRedirectTest dumpDebug
    insecureNotProxiedTest dumpDebug
    insecureProxyTest dumpDebug
    secureNotProxiedTest dumpDebug
    secureProxyTest dumpDebug


defaultTestDelegate :: ProxySettings -> IO Application
defaultTestDelegate s = do
  -- delegate everything but /status/418
  let handleFunnyStatus req = rawPathInfo req /= "/status/418"
      dummyApp _ respond = respond $ responseLBS status500 [] "I should have been proxied"
  manager <- mkBadTlsManager
  return $ delegateToProxy s manager handleFunnyStatus dummyApp


httpBinHost :: HttpBinFixture -> ByteString
httpBinHost fixture = encodeUtf8 $ hAddr $ handleOf @"tmp-http-bin" Proxy fixture


nginxHost :: ReverseProxyFixture -> ByteString
nginxHost fixture = encodeUtf8 $ hAddr $ handleOf @"nginx-test" Proxy fixture


redirectApp :: HttpBinFixture -> IO Application
redirectApp fixture = defaultTestDelegate $ tmpHostSettings (httpBinHost fixture) redirectTestSettings


testWithInsecureProxy' :: ((HttpBinFixture, Port) -> IO a) -> IO a
testWithInsecureProxy' = TmpProc.testWithApplication onlyHttpBin httpBinApp


httpBinApp :: HttpBinFixture -> IO Application
httpBinApp fixture = defaultTestDelegate $ tmpHostSettings (httpBinHost fixture) defaultTestSettings


nginxApp :: ReverseProxyFixture -> IO Application
nginxApp fixture = defaultTestDelegate $ tmpHostSettings (nginxHost fixture) defaultTestSettings


testWithSecureProxy' :: ((ReverseProxyFixture, Port) -> IO a) -> IO a
testWithSecureProxy' action = withCertPathsInTmp' $ \cp -> do
  let tls = tlsSettings (certificatePath cp) (keyPath cp)
  TmpProc.testWithTLSApplication tls nginxAndHttpBin nginxApp action


testWithInsecureProxy :: (Port -> IO ()) -> IO ()
testWithInsecureProxy = testWithApplication (defaultTestDelegate defaultTestSettings)


testWithInsecureRedirects' :: ((HttpBinFixture, Port) -> IO ()) -> IO ()
testWithInsecureRedirects' = TmpProc.testWithApplication onlyHttpBin redirectApp


httpBinHostBuilder :: HttpBinFixture -> RequestBuilder -> RequestBuilder
httpBinHostBuilder fixture builder = builder {rbHost = httpBinHost fixture}


nginxHostBuilder :: ReverseProxyFixture -> RequestBuilder -> RequestBuilder
nginxHostBuilder fixture builder = builder {rbHost = nginxHost fixture}


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
          let shouldNotMatch (f, p) = assertNeq p $ modifier $ httpBinHostBuilder f nil
          it (scheme ++ " " ++ title) shouldNotMatch


insecureRedirectTest :: Bool -> Spec
insecureRedirectTest debug =
  let scheme = "HTTP"
      desc = "Proxy over " ++ scheme ++ " with too many redirects differs"
      assertNeq = onDirectAndProxy assertHttpRepliesDiffer debug
   in aroundAll testWithInsecureRedirects' $ describe desc $ do
        for_ testOverRedirectedRequests $ \(title, modifier) -> do
          let shouldNotMatch (f, p) = assertNeq p $ modifier $ httpBinHostBuilder f nil
          it (scheme ++ " " ++ title) shouldNotMatch


insecureProxyTest :: Bool -> Spec
insecureProxyTest debug =
  let scheme = "HTTP"
      desc = "Simple " ++ scheme ++ " proxying:"
      assertEq = onDirectAndProxy assertHttpRepliesAreEq debug
   in aroundAll testWithInsecureProxy' $ describe desc $ do
        for_ testRequests $ \(title, modifier) -> do
          let shouldMatch (f, p) = assertEq p $ modifier $ httpBinHostBuilder f nil
          it (scheme ++ " " ++ title) shouldMatch


secureNotProxiedTest :: Bool -> Spec
secureNotProxiedTest debug =
  let scheme = "HTTPS"
      desc = "Proxy on " ++ scheme ++ " should fail"
      assertNeq = onDirectAndProxy assertHttpRepliesDiffer debug
   in aroundAll testWithSecureProxy' $ describe desc $ do
        for_ testNotProxiedRequests $ \(title, modifier) -> do
          let shouldNotMatch (f, p) = assertNeq p $ modifier $ nginxHostBuilder f sNil
          it (scheme ++ " " ++ title) shouldNotMatch


sNil :: RequestBuilder
sNil = secure nil


-- let shouldNotMatch (f, p) = assertNeq p $ modifier $ tmpHostBuilder f def'
-- it (scheme ++ " " ++ title) shouldNotMatch

secureProxyTest :: Bool -> Spec
secureProxyTest debug =
  let scheme = "HTTPS"
      desc = "Simple " ++ scheme ++ " proxying:"
      assertEq = onDirectAndProxy assertHttpRepliesAreEq debug
   in aroundAll testWithSecureProxy' $ describe desc $ do
        for_ testRequests $ \(title, modifier) -> do
          let shouldMatch (f, p) = assertEq p $ modifier $ nginxHostBuilder f sNil
          it (scheme ++ " " ++ title) shouldMatch


-- let shouldMatch (f, p) = assertEq p $ modifier $ tmpHostBuilder f def'
-- it (scheme ++ " " ++ title) shouldMatch

type ReverseProxyFixture = HandlesOf '[NginxGateway, HttpBin]


aGateway :: NginxGateway
aGateway =
  NginxGateway
    { ngCommonName = "localhost"
    , ngTargetPort = 80
    , ngTargetName = "tmp-http-bin"
    }


nginxAndHttpBin :: HList '[NginxGateway, HttpBin]
nginxAndHttpBin = aGateway &:& HttpBin


setupReverseProxy :: IO ReverseProxyFixture
setupReverseProxy = startupAll nginxAndHttpBin


withReverseProxy :: SpecWith ReverseProxyFixture -> Spec
withReverseProxy = beforeAll setupReverseProxy . afterAll terminateAll


-- | A data type representing a connection to a HttpBin server.
data HttpBin = HttpBin


type HttpBinFixture = HandlesOf '[HttpBin]


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
