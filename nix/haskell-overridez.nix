let
  pkgs = import <nixpkgs> {};
  overridez = fetchTarball {
    url = "https://github.com/adetokunbo/haskell-overridez/archive/v0.10.3.0.tar.gz";
    sha256 = "1mfnjksb0n3jbbsqi7g7jd3qn0c10s5q4dg1lqx3fhqw46yvr7j7";
  };
in
  import overridez { inherit pkgs; }

