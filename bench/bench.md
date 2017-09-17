[sfold](https://github.com/tonyday567/sfold)
===

[![Build Status](https://travis-ci.org/tonyday567/sfold.svg)](https://travis-ci.org/tonyday567/sfold) [![Hackage](https://img.shields.io/hackage/v/sfold.svg)](https://hackage.haskell.org/package/sfold) [![lts](https://www.stackage.org/package/sfold/badge/lts)](http://stackage.org/lts/package/sfold) [![nightly](https://www.stackage.org/package/sfold/badge/nightly)](http://stackage.org/nightly/package/sfold)

`sfold` is a composable stream library


results
---

```include
other/results.md
```

workflow
===

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/sfold-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md bench/bench.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i bench/bench.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
~~~
