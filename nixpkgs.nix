{ system ? builtins.currentSystem,
  nixBase ? "18.09"
}:
let
  nixPkgsPath = ./nix + "/${nixBase}.nix";
  nixpkgs = import nixPkgsPath;
in
  import nixpkgs { inherit system; }
