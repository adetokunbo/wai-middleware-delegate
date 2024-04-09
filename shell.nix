let
  project = import ./default.nix {};
  lib = pkgs.lib;
  pkgs = project.pkgs;
in
  project.shellFor {
    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    packages = ps: lib.attrValues (pkgs.haskell-nix.haskellLib.selectProjectPackages ps);

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal = "latest";
      hlint = "3.6.1";
      haskell-language-server = "latest";
      ghcid = "latest";
      fourmolu = "0.14.0.0";
      cabal-fmt = "latest";
    };
    # See overlays/tools.nix for more details

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # exactDeps = true;
  }
