set -e
cabal run --verbose=0 preprocess.hs < sample.md > src/sample.md
mdbook build
mdbook serve
