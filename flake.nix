{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
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
        hsPkgs = pkgs.haskell-nix.cabalProject {
          src = ./.;
          compiler-nix-name = "ghc8107";
        };
      in
      {
        packages.default = hsPkgs.projectCross.ghcjs.hsPkgs.stuff.components.library;
      });
}
