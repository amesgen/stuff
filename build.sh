#!/usr/bin/env bash
set -e

wasm32-wasi-cabal build
wasm32-wasi-cabal list-bin exe:stuff
STUFF_WASM="$(wasm32-wasi-cabal list-bin exe:stuff)"

rm -rf dist
mkdir -p dist

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
  --input "$STUFF_WASM" --output "dist/ghc_wasm_jsffi.js"
cp "$STUFF_WASM" dist/bin.wasm
cp index.js dist/
wasmtime --dir .::/ "$(wasm32-wasi-cabal list-bin exe:pregen)" dist/index.html
