#!/usr/bin/env bash

cp /home/alexander/git/stuff/Main.hs Main.hs || true
x86_64-w64-mingw32-ghc -package ghc -package ghc-boot Main.hs Standalone.hs || exit 1
fd -e dll . /home/alexander/git/stuff/result/bin/ -x cp --no-preserve=mode,ownership {} .
wine64 Main.exe 2>&1 | rg '32 bit pseudo relocation'
