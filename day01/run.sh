#! /bin/bash

set -euxo pipefail

ghc Main.hs -o main.exe && ./main.exe
