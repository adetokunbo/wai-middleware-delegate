let
  overridez = import ./nix/haskell-overridez.nix;
  addThisPackage = oldPkgs: haskellPackagesNew: haskellPackagesOld:
    with oldPkgs.haskell.lib;
    {
      wai-middleware-delegate = haskellPackagesNew.callPackage ./default.nix {};
    };
in
  [
    (newPkgs: oldPkgs: {
       haskellPackages = oldPkgs.haskellPackages.override {
         overrides = overridez.combineAllIn ./nix [(addThisPackage oldPkgs)];
       };
    })
  ]
