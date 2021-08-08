let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix {
    sourcesOverride = { hackage = sources.hackageNix; };
  };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;

in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "stuff";
    src = ./.;
  };
  compiler-nix-name = "ghc8105";
}
