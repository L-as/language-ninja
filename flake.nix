{
  description = "language-ninja";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=dfc501756b09fa40f9eeb4a1c12ccd225ba3e3d8";

  outputs = { self, nixpkgs }:
  let
    supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
    perSystem = nixpkgs.lib.genAttrs supportedSystems;
    nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ self.overlay ]; };
  in
  {
    haskellOverlay = pkgs:
      let lib = pkgs.haskell.lib.compose; in
      final: prev: {
        monad-mock = lib.overrideCabal (drv: {
          sha256 = null;
          src = pkgs.fetchFromGitHub {
            owner = "chrismanning";
            repo = "monad-mock";
            rev = "96129e57cead06aa9a8888369f35056266eb3e44";
            sha256 = "H6RRNnYVdgrO+jBzi5C8yxlwZwupA2ofqJ9O+8HI7Ro=";
          };
          broken = false;
        }) prev.monad-mock;
        language-ninja = final.callPackage ./language-ninja.nix {};
      };

    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override {
        overrides = self.haskellOverlay final;
      };
    };

    packages = perSystem (system: {
      inherit ((nixpkgsFor system).haskellPackages) language-ninja;
    });
    defaultPackage = perSystem (system: self.packages.${system}.language-ninja);

    devShell = perSystem (system:
      let pkgs = nixpkgsFor system; in
      pkgs.haskellPackages.shellFor {
        packages = p: [ p.language-ninja ];
        withHoogle = true;
        buildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
      }
    );
  };
}

