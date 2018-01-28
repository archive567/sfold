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

    machines                7.869e5 1.182e6 1.315e5 1.305e5 5.262e61.292e5 cycles
    pipe                    1.164e5 1.056e5 1.787e5 1.007e5 1.838e51.155e5 cycles
    pipe - state            1.606e7 1.609e7 1.844e7 1.879e7 1.659e71.539e7 cycles
    pipe - bad state        4.583e6 3.562e6 3.534e6 3.529e6 3.506e63.542e6 cycles
    foldl                   5.137e4 3.854e4 4.033e4 4.024e4 3.997e43.650e4 cycles
    pipe & skolems          1.564e5 1.041e5 1.979e5 9.788e4 8.823e59.727e4 cycles
    just a fold             7.305e4 2.534e4 2.528e4 2.573e4 2.559e42.225e4 cycles

workflow
========

    stack build --test --exec "$(stack path --local-install-root)/bin/sfold-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md bench/bench.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i bench/bench.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
