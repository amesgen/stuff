{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShells.default = self.shellWithArgs.${system} { };
        shellWithArgs = { a ? true, b ? 42, c ? "hello world", d ? (e: f: e + f) }:
          pkgs.mkShell {
            shellHook = ''
              echo ${toString a}
              echo ${toString b}
              echo ${c}
              echo ${toString (d 4 5)}
            '';
          };
      }
    );
}
