[sfold](https://github.com/tonyday567/sfold)
============================================

[![Build
Status](https://travis-ci.org/tonyday567/sfold.svg)](https://travis-ci.org/tonyday567/sfold)
[![Hackage](https://img.shields.io/hackage/v/sfold.svg)](https://hackage.haskell.org/package/sfold)
[![lts](https://www.stackage.org/package/sfold/badge/lts)](http://stackage.org/lts/package/sfold)
[![nightly](https://www.stackage.org/package/sfold/badge/nightly)](http://stackage.org/nightly/package/sfold)

Various folding ideas and experiments

sfold
---

`sfold` is a composable stream library

sfold-bench
---

Some performance benchmarks for folding. See [bench.md](bench.md)


    stack build --test --exec "$(stack path --local-install-root)/bin/sfold-bench"

words
---

Experiment in word counting. See [bench.md](bench.md)


    stack build --test --exec "$(stack path --local-install-root)/bin/words-example"

foldls
---

Some useful foldl folds
