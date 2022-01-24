Bindings to Rust's [regular expression library](https://github.com/rust-lang/regex). See [here](https://github.com/rust-lang/regex/tree/master/regex-capi#c-api-for-rusts-regex-engine) for installation instructions. You'll need to put `librure.so` or `librure.dylib` etc. where libraries go on your system.

Lower-level bindings are exhaustive; higher-level bindings no not include
capture groups.

# Performance

As of 0.1.0.3:

```
benchmarking rure/matches
time                 334.6 ns   (334.1 ns .. 335.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 333.8 ns   (333.5 ns .. 334.2 ns)
std dev              1.247 ns   (1.058 ns .. 1.649 ns)

benchmarking tdfa/matches
time                 1.180 μs   (1.180 μs .. 1.181 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.179 μs   (1.179 μs .. 1.180 μs)
std dev              2.029 ns   (1.607 ns .. 2.851 ns)
```
