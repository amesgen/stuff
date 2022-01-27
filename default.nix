let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "stuff";
      src = ./.;
    };
    compiler-nix-name = "ghc8107";
  };
in
rec {
  exe = hsPkgs.stuff.components.exes.stuff;
  macOS = pkgs.runCommand "stuff-macOS"
    { buildInputs = [ pkgs.macdylibbundler ]; } ''
    mkdir -p $out/bin
    cp ${exe}/bin/stuff $out/bin/stuff
    chmod 755 $out/bin/stuff
    dylibbundler -b -x $out/bin/stuff -d $out/bin -p '@executable_path'
  '';
}
