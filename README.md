# Pre-rendering for Miso components

Relevant Miso PR: https://github.com/dmjio/miso/pull/783

## How to build and test

 1. Enter the Nix shell (`nix develop` or `direnv allow`).
 2. Run `wasm32-wasi-cabal update`.
 3. Run `./build.sh`.
 4. Run `miniserve dist`.
 5. Visit http://localhost:8080/index.html

The expectation is that the page briefly displays `Loading...` and then `Loaded`, plus a button that can increment a counter.

Originally, this branch was used a minimal reproducible example for bugs in the aforementioned PR, but all of these have now been fixed by @dmjio :tada:
