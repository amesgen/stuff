let pkgs = import ./nix/pkgs.nix;

in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "stuff";
    src = ./.;
  };
  compiler-nix-name = "ghc8105";
}
