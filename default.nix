{ mkDerivation, async, base, blaze-builder, bytestring
, case-insensitive, conduit, conduit-extra, http-client
, http-conduit, http-types, stdenv, streaming-commons, text, wai
, wai-conduit
}:
mkDerivation {
  pname = "wai-middleware-delegate";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base blaze-builder bytestring case-insensitive conduit
    conduit-extra http-client http-conduit http-types streaming-commons
    text wai wai-conduit
  ];
  description = "WAI middleware that proxies requests to other websites";
  license = stdenv.lib.licenses.bsd3;
}
