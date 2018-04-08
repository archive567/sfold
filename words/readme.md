words
===

[![Build Status](https://travis-ci.org/tonyday567/words.png)](https://travis-ci.org/tonyday567/words)

Experiments in word counting.

See https://tonyday567.github.io/words/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/words-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/words.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
