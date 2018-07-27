{ system ? "x86_64-darwin"
}:
let
  overlays = import ./overlays.nix;
  nixpkgs = import ./nix/18.09pre146471.nix;
  pkgs = import <nixpkgs> { inherit overlays system; };
in
 {
  inherit (pkgs.haskellPackages) wai-middleware-delegate;
 }
