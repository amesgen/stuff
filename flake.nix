{
  inputs = {
    haskellNixBad.url = "github:input-output-hk/haskell.nix/008e8ccff65d8734098d63ca11b9208522781c78";
    haskellNixGood.url = "github:input-output-hk/haskell.nix/e98545327a92d009ddc47a9a5b8a71c7ced7df7b";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        buildWith = haskellNix:
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
          project.projectCross.ucrt64.hsPkgs.stuff.components.exes.stuff;
      in
      {
        packages.bad = buildWith inputs.haskellNixBad;
        packages.good = buildWith inputs.haskellNixGood;
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
