# wai-middleware-delegate [![CircleCI](https://circleci.com/gh/adetokunbo/wai-middleware-delegate.svg?style=svg)](https://circleci.com/gh/adetokunbo/wai-middleware-delegate) [![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/wai-middleware-delegate/blob/master/LICENSE)

__wai-middleware-delegate__ is a [WAI](https://hackage.haskell.org/package/wai) middleware that allows requests to be handled by a delegate application that proxies requests to another server.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.Default                    (Default (..))
import           Network.HTTP.Client.TLS         (newTlsManager)
import           Network.HTTP.Types              (status500)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Delegate (ProxySettings (..),
                                                  delegateToProxy)

sampleSettings :: ProxySettings
sampleSettings = def { proxyHost = "httpbin.org" }

-- | Create an application that proxies every request to httpbin.org
httpBinDelegate :: ProxySettings -> IO Application
httpBinDelegate s = do
  -- delegate everything!
  let takeItAll = const True
      dummyApp _ respond = respond $ responseLBS status500 [] "I should have been proxied"

  manager <- newTlsManager
  return $ delegateToProxy s manager (takeItAll) dummyApp

main :: IO ()
main = httpBinDelegate sampleSettings >>= run 3000

```
