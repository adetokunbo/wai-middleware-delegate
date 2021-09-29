{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Control.Monad                   (when)
import           Data.Default                    (Default (..))
import           Data.Foldable                   (for_)
import           Network.HTTP.Client.TLS         (newTlsManager)
import           Network.HTTP.Types              (status500)
import           Network.Wai                     (Application, rawPathInfo,
                                                  responseLBS)
import           Network.Wai.Handler.Warp        (Port, testWithApplication)
import           System.Environment              (lookupEnv)

import           Network.Wai.Middleware.Delegate (ProxySettings (..),
                                                  delegateToProxy)

import           Test.Fetch                      (fetch)
import           Test.Hspec
import           Test.HttpReply
import           Test.TestRequests               (RequestBuilder (..),
                                                  buildRequest,
                                                  testNotProxiedRequests,
                                                  testOverRedirectedRequests,
                                                  testRequests)
import           Test.WithExtras                 (defaultTlsSettings,
                                                  testWithTLSApplication)

defaultTestSettings :: ProxySettings
defaultTestSettings = def { proxyHost = "httpbin.org", proxyTimeout = 2 }

redirectTestSettings :: ProxySettings
redirectTestSettings = defaultTestSettings { proxyRedirectCount = 2 }

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  dumpDebug' <- lookupEnv "DEBUG"
  let dumpDebug = maybe False (const True) dumpDebug'
  hspec $ do
    -- TOOD: reenable when the public /redirect link on http-bin is working ok
    -- insecureRedirectTest dumpDebug
    insecureProxyTest dumpDebug
    insecureNotProxiedTest dumpDebug
    secureProxyTest dumpDebug
    secureNotProxiedTest dumpDebug

defaultTestDelegate :: ProxySettings -> IO Application
defaultTestDelegate s = do
  -- delegate everything but /status/418
  let handleFunnyStatus = \req -> rawPathInfo req /= "/status/418"
      dummyApp _ respond = respond $ responseLBS status500 [] "I should have been proxied"

  manager <- newTlsManager
  return $ delegateToProxy s manager (handleFunnyStatus) dummyApp

testWithInsecureProxy :: (Port -> IO ()) -> IO ()
testWithInsecureProxy = testWithApplication (defaultTestDelegate defaultTestSettings)

testWithInsecureRedirects :: (Port -> IO ()) -> IO ()
testWithInsecureRedirects = testWithApplication (defaultTestDelegate redirectTestSettings)

testWithSecureProxy :: (Port -> IO ()) -> IO ()
testWithSecureProxy = testWithTLSApplication defaultTlsSettings (defaultTestDelegate defaultTestSettings)

onDirectAndProxy :: (HttpReply -> HttpReply -> IO ()) -> Bool -> Int -> RequestBuilder -> IO ()
onDirectAndProxy f debug testProxyPort builder = do
  let proxiedBuilder = builder { rbHost = "localhost", rbPort = Just testProxyPort }
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
  in
    around testWithInsecureProxy $ describe desc $ do
    for_ testNotProxiedRequests $ \(title, modifier) ->
      it (scheme ++ " " ++ title) $ \port -> assertNeq port $ modifier def

insecureRedirectTest :: Bool -> Spec
insecureRedirectTest debug =
  let scheme = "HTTP"
      desc = "Proxy over " ++ scheme ++ " with too many redirects differs"
      assertNeq = onDirectAndProxy assertHttpRepliesDiffer debug
  in
    around testWithInsecureRedirects $ describe desc $ do
    for_ testOverRedirectedRequests $ \(title, modifier) ->
      it (scheme ++ " " ++ title) $ \port -> assertNeq port $ modifier def

insecureProxyTest :: Bool -> Spec
insecureProxyTest debug =
  let scheme = "HTTP"
      desc = "Simple " ++ scheme ++ " proxying:"
      assertEq = onDirectAndProxy assertHttpRepliesAreEq debug
  in
    around testWithInsecureProxy $ describe desc $ do
    for_ testRequests $ \(title, modifier) ->
      it (scheme ++ " " ++ title) $ \port -> assertEq port $ modifier def

secureNotProxiedTest :: Bool -> Spec
secureNotProxiedTest debug =
  let
    scheme = "HTTPS"
    desc = "Proxy on " ++ scheme ++ " should fail"
    assertNeq = onDirectAndProxy assertHttpRepliesDiffer debug
    def' = def { rbSecure = True }
  in
    around testWithSecureProxy $ describe desc $ do
    for_ testNotProxiedRequests $ \(title, modifier) ->
      it (scheme ++ " " ++ title) $ \port -> assertNeq port $ modifier def'

secureProxyTest :: Bool -> Spec
secureProxyTest debug =
  let
    scheme = "HTTPS"
    desc = "Simple " ++ scheme ++ " proxying:"
    assertEq = onDirectAndProxy assertHttpRepliesAreEq debug
    def' = def { rbSecure = True }
  in
    around testWithSecureProxy $ describe desc $ do
    for_ testRequests $ \(title, modifier) ->
      it (scheme ++ " " ++ title) $ \port -> assertEq port $ modifier def'
