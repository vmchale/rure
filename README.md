Bindings to Rust's [regular expression library](https://github.com/rust-lang/regex). See [here](https://github.com/rust-lang/regex/tree/master/regex-capi#c-api-for-rusts-regex-engine) for installation instructions.

Lower-level bindings are exhaustive; higher-level bindings no not include
capture groups.

# Performance

Compared to [regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)
performance is slightly better:

```
benchmarking rure/matches
time                 765.0 ns   (756.8 ns .. 771.9 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 754.7 ns   (750.3 ns .. 761.3 ns)
std dev              17.98 ns   (13.77 ns .. 24.42 ns)
variance introduced by outliers: 31% (moderately inflated)

benchmarking tdfa/matches
time                 1.186 μs   (1.178 μs .. 1.194 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.178 μs   (1.171 μs .. 1.184 μs)
std dev              20.63 ns   (17.24 ns .. 25.28 ns)
variance introduced by outliers: 19% (moderately inflated)
```
