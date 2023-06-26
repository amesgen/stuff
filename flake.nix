{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.follows = "haskellNix/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let

      pkgs = import inputs.haskellNix.inputs.nixpkgs-unstable {
        inherit system;
        inherit (inputs.haskellNix) config;
        overlays = [ inputs.haskellNix.overlay ];
      };
      hsPkgs = pkgs.haskell-nix.cabalProject {
        src = ./.;
        compiler-nix-name = "ghc96";
      };
    in
    {
      packages.default = hsPkgs.stuff.components.exes.stuff;
    });
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
