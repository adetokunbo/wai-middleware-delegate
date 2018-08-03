module Test.WithExtras
  ( defaultTlsSettings
  , testWithTLSApplication
  , testWithTLSApplicationSettings
  , withTLSApplication
  , withTLSApplicationSettings
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad                     (when)

import           Network.Socket                    (Socket, close)
import           Network.Wai                       (Application)
import           Network.Wai.Handler.Warp.Internal (settingsBeforeMainLoop)

import           Network.Wai.Handler.Warp          (Port, Settings,
                                                    defaultShouldDisplayException,
                                                    defaultSettings,
                                                    openFreePort)
import           Network.Wai.Handler.WarpTLS       (TLSSettings,
                                                    runTLSSocket, tlsSettings)

-- | The settings used in the integration tests
defaultTlsSettings :: TLSSettings
defaultTlsSettings = tlsSettings "test/certificate.pem" "test/key.pem"

-- | Runs the given 'Application' on a free port. Passes the port to the given
-- operation and executes it, while the 'Application' is running. Shuts down the
-- server before returning.
withTLSApplication :: TLSSettings -> IO Application -> (Port -> IO a) -> IO a
withTLSApplication = withTLSApplicationSettings defaultSettings

-- | 'withTLSApplication' with given 'Settings'. This will ignore the port value
-- set by 'setPort' in 'Settings'.
withTLSApplicationSettings :: Settings -> TLSSettings -> IO Application -> (Port -> IO a) -> IO a
withTLSApplicationSettings settings' tlsSettings' mkApp action = do
  app <- mkApp
  withFreePort $ \ (port, sock) -> do
    started <- mkWaiter
    let settings =
          settings' {
            settingsBeforeMainLoop
              = notify started () >> settingsBeforeMainLoop settings'
          }
    result <- race
      (runTLSSocket tlsSettings' settings sock app)
      (waitFor started >> action port)
    case result of
      Left () -> throwIO $ ErrorCall "Unexpected: runSettingsSocket exited"
      Right x -> return x

testWithTLSApplication :: TLSSettings -> IO Application -> (Port -> IO a) -> IO a
testWithTLSApplication = testWithTLSApplicationSettings defaultSettings

testWithTLSApplicationSettings :: Settings -> TLSSettings -> IO Application -> (Port -> IO a) -> IO a
testWithTLSApplicationSettings settings tlsSettings' mkApp action = do
  callingThread <- myThreadId
  app <- mkApp
  let wrappedApp request respond =
        app request respond `catch` \ e -> do
          when
            (defaultShouldDisplayException e)
            (throwTo callingThread e)
          throwIO e
  withTLSApplicationSettings settings tlsSettings' (return wrappedApp) action

data Waiter a
  = Waiter {
    notify  :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }

-- | Like 'openFreePort' but closes the socket before exiting.
withFreePort :: ((Port, Socket) -> IO a) -> IO a
withFreePort = bracket openFreePort (close . snd)
