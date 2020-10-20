{ system ? builtins.currentSystem
, nixBase ? "lts-haskell.14.0"
}:
let
  overridez = import ./nix/haskell-overridez.nix;
  pkgsMake = import ./nix/fetchPkgsMake.nix {};
  nixpkgsVersionFile = (./. + "/nix/${nixBase}.nix");
  nixpkgsVersion = import nixpkgsVersionFile;

  pkgsMakeHaskellOverridez = pkgs: overridez.allIn ./nix;
  pkgsMakeArgs = {
    nixpkgsRev = nixpkgsVersion.rev;
    nixpkgsSha256 = nixpkgsVersion.sha256;
    haskellArgs = {
      ghcVersion = "ghc865";
      overrides = pkgsMakeHaskellOverridez;
      extraOverrides = pkgsMakeHaskellOverridez;
      envMoreTools = [
        pkgs.haskellPackages.apply-refact
        pkgs.haskellPackages.cabal2nix
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.ghcid
        pkgs.haskellPackages.ghci-pretty
        pkgs.haskellPackages.hlint
        pkgs.haskellPackages.hoogle
        pkgs.haskellPackages.stylish-cabal
        pkgs.haskellPackages.stylish-haskell
      ];
    };
  };
  pkgs = import ./nix/fetchNixPkgs.nix { inherit nixpkgsVersionFile; };

in

pkgsMake pkgsMakeArgs ({ call, lib, ... }: rec {
  wai-middleware-delegate = call.haskell.cabal2nix.lib ./.;
})
