Bindings to Rust's [regular expression library](https://github.com/rust-lang/regex). See [here](https://github.com/rust-lang/regex/tree/master/regex-capi#c-api-for-rusts-regex-engine) for installation instructions. You'll need to put `librure.so` or `librure.dylib` etc. where libraries go on your system.

Lower-level bindings are exhaustive; higher-level bindings no not include
capture groups.
