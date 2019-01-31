{ system ? builtins.currentSystem
, nixBase ? "18.09"
}:
let
  overridez = import ./nix/haskell-overridez.nix;
  pkgsMakeHaskellOverridez = pkgs: overridez.allIn ./nix;
  pkgsMake = import ./nix/fetchPkgsMake.nix {};
  pkgsMakeArgs = {
    nixpkgsRev = "a4c4cbb613cc3e15186de0fdb04082fa7e38f6a0";
    nixpkgsSha256 = "1lagfycy2lvfc8cdxk98dz2rxjlrbmv9hj42x0x40sy66bck1w0y";
    haskellArgs = {
      overrides = pkgsMakeHaskellOverridez;
      extraOverrides = pkgsMakeHaskellOverridez;
      envMoreTools = [
        pkgs.haskellPackages.cabal2nix
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.ghcid
        pkgs.haskellPackages.hlint
        pkgs.haskellPackages.hoogle
        pkgs.haskellPackages.stylish-haskell
      ];
    };
  };
  pkgs = import ./nixpkgs.nix { inherit nixBase system; };

in

pkgsMake pkgsMakeArgs ({ call, lib, ... }: rec {
  wai-middleware-delegate = call.haskell.cabal2nix.lib ./.;
})
