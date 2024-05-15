{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Test.NginxTest
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Test.NginxTest
  ( -- * data types
    NginxTest (..)
  , NginxPrep (..)

    -- * ping via https
  , pingHttps
  )
where

import qualified Data.ByteString.Char8 as C8
import Data.Data (Proxy (..))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HC
import Network.HTTP.Types.Header (hHost)
import Network.HTTP.Types.Status (statusCode)
import Paths_wai_middleware_delegate (getDataDir)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.ByteString (getEffectiveGroupID, getEffectiveUserID)
import System.Posix.Types (GroupID, UserID)
import System.TmpProc
  ( HostIpAddress
  , Pinged (..)
  , Preparer (..)
  , Proc (..)
  , ProcHandle (..)
  , SlimHandle (..)
  , SvcURI
  , ToRunCmd (..)
  , toPinged
  )
import Test.Certs.Temp (CertPaths (..), defaultConfig, generateAndStore)
import Text.Mustache
  ( ToMustache (..)
  , automaticCompile
  , object
  , substitute
  , (~>)
  )


-- | Run Nginx as a temporary process.
instance Proc NginxTest where
  -- use this linuxserver.io nginx as it is setup to allow easy override of
  -- config
  type Image NginxTest = "lscr.io/linuxserver/nginx"
  type Name NginxTest = "nginx-test"
  uriOf = httpUri
  runArgs = []
  reset _ = pure ()
  ping = pingHttps


instance ToRunCmd NginxTest NginxPrep where
  toRunCmd = toRunCmd'


instance Preparer NginxTest NginxPrep where
  prepare = prepare'
  tidy = tidy'


-- | Configures launch of a container thats uses nginx as a reverse proxy.
data NginxTest = NginxTest
  { ntCommonName :: !Text
  , ntTargetPort :: !Int
  , ntTargetName :: !Text
  }
  deriving (Eq, Show)


instance ToMustache NginxTest where
  toMustache nt =
    object
      [ "commonName" ~> ntCommonName nt
      , "targetPort" ~> ntTargetPort nt
      , "targetName" ~> ntTargetName nt
      ]


-- | Values obtained while in preparation to launch the nginx container
data NginxPrep = NginxPrep
  { npUserID :: !UserID
  , npGroupID :: !GroupID
  , npVolumeRoot :: !FilePath
  }
  deriving (Eq, Show)


instance ToMustache NginxPrep where
  toMustache np =
    object
      [ "targetDir" ~> npVolumeRoot np
      ]


templateName :: FilePath
templateName = "nginx-test.conf.mustache"


toConfCertsDirs :: FilePath -> (FilePath, FilePath)
toConfCertsDirs topDir = (topDir </> "conf", topDir </> "certs")


dockerCertsDir :: FilePath
dockerCertsDir = "/etc/tmp-proc/certs"


dockerConf :: FilePath
dockerConf = "/data/conf/nginx.conf"


createWorkingDirs :: IO FilePath
createWorkingDirs = do
  tmpDir <- getCanonicalTemporaryDirectory
  topDir <- createTempDirectory tmpDir "nginx-test"
  let (confDir, certsDir) = toConfCertsDirs topDir
  createDirectory confDir
  createDirectory certsDir
  pure topDir


tidy' :: NginxTest -> NginxPrep -> IO ()
tidy' _ np = removeDirectoryRecursive $ npVolumeRoot np


toRunCmd' :: NginxTest -> NginxPrep -> [Text]
toRunCmd' _ np =
  -- specify user ID and group ID to fix volume mount permissions
  -- mount volume /etc/tmp-proc/certs as target-dir/certs
  -- mount volume /etc/tmp-proc/nginx as target-dir/nginx
  let (confDir, certsDir) = toConfCertsDirs $ npVolumeRoot np
      confPath = confDir </> "nginx.conf"
      envArg name v =
        [ "-e"
        , name ++ "=" ++ show v
        ]
      volumeArg actualPath hostedPath =
        [ "-v"
        , actualPath ++ ":" ++ hostedPath
        ]
      confArg = volumeArg confPath $ dockerConf ++ ":ro"
      certsArg = volumeArg certsDir dockerCertsDir
      puidArg = envArg "PUID" $ npUserID np
      guidArg = envArg "GUID" $ npGroupID np
   in Text.pack <$> confArg ++ certsArg ++ puidArg ++ guidArg


ifHasHandle :: [SlimHandle] -> Text -> (SlimHandle -> IO a) -> IO a
ifHasHandle hs name action = case find ((== name) . shName) hs of
  Nothing -> error $ "could not find host " <> show name
  Just sh -> action sh


-- Prepare
-- expand the template with commonName to target-dir/nginx
-- create certs with commonName to target-dir/certs
-- used fixed cert basenames (certificate.pem and key.pem)
prepare' :: [SlimHandle] -> NginxTest -> IO NginxPrep
prepare' hs nt@NginxTest {ntTargetName = name} = ifHasHandle hs name $ \_ -> do
  templateDir <- (</> "templates") <$> getDataDir
  compiled <- automaticCompile [templateDir] templateName
  case compiled of
    Left err -> error $ "the template did not compile:" ++ show err
    Right template -> do
      npVolumeRoot <- createWorkingDirs
      npUserID <- getEffectiveUserID
      npGroupID <- getEffectiveGroupID
      let (confDir, cpDir) = toConfCertsDirs npVolumeRoot
          cp =
            CertPaths
              { cpKey = "key.pem"
              , cpCert = "certificate.pem"
              , cpDir
              }
          np = NginxPrep {npUserID, npGroupID, npVolumeRoot}
      generateAndStore cp defaultConfig
      Text.writeFile (confDir </> "nginx.conf") $ substitute template (nt, np)
      pure np


-- | Make a uri access the http-bin server.
httpUri :: HostIpAddress -> SvcURI
httpUri ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


pingHttps :: ProcHandle a -> IO Pinged
pingHttps handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- httpsGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


-- | Determine the status from a secure Get to host localhost.
httpsGet :: ProcHandle a -> Text -> IO Int
httpsGet handle urlPath = do
  let theUri = "https://" <> hAddr handle <> "/" <> Text.dropWhile (== '/') urlPath
      tlsSettings = TLSSettingsSimple True False False
  manager <- HC.newTlsManagerWith $ HC.mkManagerSettings tlsSettings Nothing
  getReq <- HC.parseRequest $ Text.unpack theUri
  let withHost = getReq {HC.requestHeaders = [(hHost, "localhost")]}
  statusCode . HC.responseStatus <$> HC.httpLbs withHost manager
