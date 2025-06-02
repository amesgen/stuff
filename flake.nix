{
  inputs = {
    haskellNixBad.url = "github:input-output-hk/haskell.nix/da5e52c6d4ba36696776104b8e2b80f6bcf6f437";
    haskellNixGood.url = "github:input-output-hk/haskell.nix/794d2e76c7150eeebcec8f818b745c8e3ee0e4dc";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        mkShellFor = haskellNix:
          let
            pkgs = import haskellNix.inputs.nixpkgs {
              inherit system;
              inherit (haskellNix) config;
              overlays = [ haskellNix.overlay ];
            };
            project = pkgs.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc96";
            };
          in
          project.shell;
      in
      {
        devShells = {
          bad = mkShellFor inputs.haskellNixBad;
          good = mkShellFor inputs.haskellNixGood;
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
