# wai-middleware-delegate

[![Hackage](https://img.shields.io/hackage/v/wai-middleware-delegate.svg)](https://hackage.haskell.org/package/wai-middleware-delegate)
[![GitHub CI](https://github.com/adetokunbo/wai-middleware-delegate/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/wai-middleware-delegate/actions)
[![Stackage Nightly](http://stackage.org/package/wai-middleware-delegate/badge/nightly)](http://stackage.org/nightly/package/wai-middleware-delegate)
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/wai-middleware-delegate/blob/master/LICENSE)

`wai-middleware-delegate` is a [WAI][1] middleware that allows requests to be
handled by a delegate application that proxies requests to another server.


## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import  Data.Default                    (Default (..))
import  Network.HTTP.Client.TLS         (newTlsManager)
import  Network.HTTP.Types              (status500)
import  Network.Wai
import  Network.Wai.Handler.Warp        (run)
import  Network.Wai.Middleware.Delegate (ProxySettings (..), delegateToProxy)

demoSettings :: ProxySettings
demoSettings = def { proxyHost = "httpbin.org" }

-- | An trivial app that proxies every request to httpbin.org
httpBin :: ProxySettings -> IO Application
httpBin s = do

  -- delegate everything!
  let takeItAll = const True
      dummyApp _ respond = respond $
        responseLBS status500 [] "I should have been proxied"

  manager <- newTlsManager
  return $ delegateToProxy s manager takeItAll dummyApp

main :: IO ()
main = httpBin demoSettings >>= run 3000

```

[1]: https://hackage.haskell.org/package/wai
