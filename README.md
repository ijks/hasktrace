# hasktrace

A ray tracer in Haskell. For fun, probably not profit. I'm going to try using this as an
exercise in making Haskell code go fast. 🚀

To use it, run

```sh
cabal run trace
```

This should render a test scene and save the output at `test.png`.

To run benchmarks, do

```sh
cabal bench --benchmark-options='--baseline benchmarks/baseline.csv'
```

which compares against a pre-existing baseline. To update the benchmarks, e.g. when
performance has improved or new benchmarks have been added, run

```sh
cabal bench --benchmark-options='--csv benchmarks/baseline.csv'
```
