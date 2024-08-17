{
  inputs = {
    nixpkgs-good.url = "github:NixOS/nixpkgs/3259cf03626f8fd2f54c67becd531b9276885a64";
    nixpkgs-bad.url = "github:NixOS/nixpkgs/c20c714d7cdf582c9c9db6bdac37c407d33d1789";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      myShell = nixpkgs:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskellPackages.extend (hfinal: hprev: {
            stuff = hfinal.developPackage {
              root = pkgs.lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
            };
          });
        in
        hsPkgs.shellFor {
          packages = p: [ p.stuff ];
          buildInputs = [ pkgs.zlib ];
          nativeBuildInputs = [ pkgs.cabal-install ];
        };
    in
    {
      devShells = {
        good = myShell inputs.nixpkgs-good;
        bad = myShell inputs.nixpkgs-bad;
      };
    });
}
