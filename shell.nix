let pkgs = import ./nix/pkgs.nix;

in (import ./default.nix).shellFor {
  tools = { cabal = "latest"; };
  buildInputs = [ pkgs.git pkgs.ghcid ];
  withHoogle = true;
  exactDeps = true;
}
