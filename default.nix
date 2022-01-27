let
  pkgs =
    let
      sources = import ./nix/sources.nix { };
      haskellNix = import sources.haskellNix { };
      inherit (haskellNix) nixpkgsArgs;
      overlays = nixpkgsArgs.overlays ++ [
        (self: super: {
          macdylibbundler = super.macdylibbundler.overrideAttrs (old: {
            version = "custom";
            src = super.fetchFromGitHub {
              owner = "amesgen";
              repo = "macdylibbundler";
              rev = "f98d06980e718947b1d4afed3586f475056563b6";
              sha256 = "0qqgcp14vvr6r5yqj96hy3klc2dkcqyq909az30ql4y879r3xm62";
            };
          });
        })
      ];
    in
    import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
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
