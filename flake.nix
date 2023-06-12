{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs.url = "github:amesgen/nixpkgs/haskell.nix-blas";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };
        inherit (pkgs) lib;
        hsPkgs = pkgs.haskell-nix.cabalProject {
          src = ./.;
          compiler-nix-name = "ghc928";
          modules = [
            ({ pkgs, ... }: {
              packages.stuff.components.tests.stuff.libs = [
                pkgs.blas
                pkgs.blas.passthru.provider
                pkgs.liblapack
                pkgs.buildPackages.gfortran.cc
              ];
            })
          ];
        };
      in
      {
        packages = {
          native = hsPkgs.stuff.checks.stuff;
          windows = hsPkgs.projectCross.mingwW64.hsPkgs.stuff.checks.stuff;
        };
      });
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
