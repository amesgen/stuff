# Repro for pre-rendering for Miso components

Relevant Miso PR: https://github.com/dmjio/miso/pull/783

## How to build and test

 1. Enter the Nix shell (`nix develop` or `direnv allow`).
 2. Run `wasm32-wasi-cabal update`.
 3. Run `./build.sh`.
 4. Run `miniserve dist`.
 5. Visit http://localhost:8080/index.html

The expectation is that the page briefly displays `Loading...` and then `Loaded`.

The file `src/Stuff.hs` contains three variants of the same very simple Miso app:

 - `components1` (the default) and `components2`, which both do not work.
 - `noComponents`, which works, but doesn't use components.
