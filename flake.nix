{
  description = "Haskell Flake (GHC 9.10.2)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskell = pkgs.haskellPackages;
        ghc = haskell.ghc;
        hsDeps = haskell.ghcWithPackages (p: with p; [ HUnit ]);
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            ghc
            pkgs.cabal-install
            haskell.stack
            pkgs.git
            pkgs.zlib
            pkgs.pkg-config
          ];
        };

        packages.quad-solv = pkgs.haskellPackages.callCabal2nix "quad-solv" ./. {};
      });
}

