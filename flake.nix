{
  inputs = {
    nixpkgs.follows = "hls/nixpkgs";
    flake-utils.follows = "hls/flake-utils";
    hls.url = "github:amesgen/haskell-language-server/completion-ignore-deprecated";
  };
  outputs = { self, nixpkgs, flake-utils, hls }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.haskellPackages.ghc
            pkgs.cabal-install
            hls.packages.${system}.default
          ];
        };
      });
  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };
}
