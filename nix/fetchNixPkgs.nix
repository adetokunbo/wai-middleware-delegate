{ bootstrap ? import <nixpkgs> {}
, nixpkgsVersionFile
}:
let
  nixpkgsVersion = import nixpkgsVersionFile;
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgsVersion) rev sha256;
  };
in
  import src {}
