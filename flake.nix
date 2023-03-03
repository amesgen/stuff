{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.default = pkgs.hello.overrideAttrs (_: {
        name = "hello-variant";
      });
    });
  nixConfig = {
    extra-substituters = [
      "https://cache.amesgen.de/test"
    ];
    extra-trusted-public-keys = [
      "test:R29p5Eb4R+TdiZdVeDiQ1o0nlvbk1bjPENTUbHQ5ZEA="
    ];
  };
}
