{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (pkgs) lib haskell;
      hsPkgs = pkgs.haskell.packages.ghc946.extend (hfinal: hprev: {
        stuff = hfinal.developPackage {
          root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
          modifier = haskell.lib.compose.overrideCabal {
            enableLibraryProfiling = false;
            doHaddock = false;
          };
        };
      });
      inherit (haskell.lib.compose) enableCabalFlag disableCabalFlag;
      variants = {
        no-options =
          enableCabalFlag "no-options" hsPkgs.stuff;
        language-then-options =
          disableCabalFlag "no-options"
            (enableCabalFlag "language-before-options" hsPkgs.stuff);
        options-then-language =
          disableCabalFlag "no-options"
            (disableCabalFlag "language-before-options" hsPkgs.stuff);
      };
    in
    {
      packages = variants // {
        all = pkgs.linkFarm "all" variants;
      };
    }
  );
}
