{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        inherit (inputs.haskellNix) config;
        overlays = [ inputs.haskellNix.overlay ];
      };
      project = pkgs.haskell-nix.cabalProject' {
        src = ./.;
        compiler-nix-name = "ghc96";
      };
    in
    {
      inherit project;
      packages.default =
        project.projectCross.mingwW64.hsPkgs.stuff.checks.tests;
      packages.exe =
        project.projectCross.mingwW64.hsPkgs.stuff.components.tests.tests;
      devShells.default = project.shellFor {
        crossPlatforms = p: [ p.mingwW64 ];
        nativeBuildInputs = [
          pkgs.creduce
          pkgs.winePackages.minimal
          project.projectCross.mingwW64.pkgs.stdenv.cc
        ];
        withHoogle = false;
      };
    });
  nixConfig = {
    extra-substituters = [
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
