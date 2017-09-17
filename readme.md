[sfold](https://github.com/tonyday567/sfold)
============================================

[![Build
Status](https://travis-ci.org/tonyday567/sfold.svg)](https://travis-ci.org/tonyday567/sfold)
[![Hackage](https://img.shields.io/hackage/v/sfold.svg)](https://hackage.haskell.org/package/sfold)
[![lts](https://www.stackage.org/package/sfold/badge/lts)](http://stackage.org/lts/package/sfold)
[![nightly](https://www.stackage.org/package/sfold/badge/nightly)](http://stackage.org/nightly/package/sfold)

`sfold` is a composable stream library

results
-------

    machines                 3.01e6  1.04e7  2.32e5  3.00e5  2.59e5 1.81e5 cycles
    pipe                     3.61e5  1.39e5  2.05e5  1.70e5  1.82e5 1.66e5 cycles
    pipe - state             2.48e7  2.10e7  2.19e7  2.24e7  1.92e7 1.78e7 cycles
    pipe - bad state         4.34e6  3.63e6  3.55e6  3.73e6  4.08e6 3.85e6 cycles
    foldl                    4.22e6  9.09e4  9.15e4  2.33e5  1.29e5 5.29e4 cycles
    pipe & skolems           2.95e5  3.41e5  2.62e5  2.97e5  3.22e5 1.69e5 cycles
    just a fold              4.28e4  3.39e4  3.42e4  3.46e4  3.42e4 3.14e4 cycles

workflow
========

    stack build --test --exec "$(stack path --local-install-root)/bin/sfold-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md bench/bench.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i bench/bench.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
