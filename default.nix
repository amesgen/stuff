let
  sources = import ./nix/sources.nix { };
  pkgs =
    let haskellNix = import sources.haskellNix { }; in
    import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "stuff";
      src = ./.;
    };
    compiler-nix-name = "ghc8107";
  };
  macdylibbundler = pkgs.macdylibbundler.overrideAttrs (old: {
    version = "custom";
    src = sources.macdylibbundler;
  });
in
rec {
  exe = hsPkgs.stuff.components.exes.stuff;
  macOS = pkgs.runCommand "stuff-macOS"
    { buildInputs = [ macdylibbundler ]; } ''
    mkdir -p $out/bin
    cp ${exe}/bin/stuff $out/bin/stuff
    chmod 755 $out/bin/stuff
    dylibbundler -b -x $out/bin/stuff -d $out/bin -p '@executable_path'
  '';
}
