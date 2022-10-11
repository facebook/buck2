# TwoX-Hash

A Rust implementation of the [XXHash] algorithm.

[![Build Status](https://travis-ci.org/shepmaster/twox-hash.svg)](https://travis-ci.org/shepmaster/twox-hash) [![Current Version](https://img.shields.io/crates/v/twox-hash.svg)](https://crates.io/crates/twox-hash)

[Documentation](https://docs.rs/twox-hash/)

[XXHash]: https://github.com/Cyan4973/xxHash

## Examples

### With a fixed seed

```rust
use std::hash::BuildHasherDefault;
use std::collections::HashMap;
use twox_hash::XxHash64;

let mut hash: HashMap<_, _, BuildHasherDefault<XxHash64>> = Default::default();
hash.insert(42, "the answer");
assert_eq!(hash.get(&42), Some(&"the answer"));
```

### With a random seed

```rust
use std::collections::HashMap;
use twox_hash::RandomXxHashBuilder64;

let mut hash: HashMap<_, _, RandomXxHashBuilder64> = Default::default();
hash.insert(42, "the answer");
assert_eq!(hash.get(&42), Some(&"the answer"));
```

## Benchmarks

### 64-bit

|   Bytes | SipHasher (MB/s) | XXHash (MB/s) | Ratio |
|---------|------------------|---------------|-------|
|       1 |               52 |            38 |   73% |
|       4 |              210 |           148 |   70% |
|      16 |              615 |           615 |  100% |
|      32 |              914 |          1391 |  152% |
|     128 |             1347 |          3657 |  271% |
|     256 |             1414 |          5019 |  355% |
|     512 |             1546 |          6168 |  399% |
|    1024 |             1565 |          6206 |  397% |
| 1048576 |             1592 |          7564 |  475% |

|   Bytes | [FnvHasher][fnv] (MB/s) | XXHash (MB/s) | Ratio |
|---------|-------------------------|---------------|-------|
|       1 |                    1000 |            38 |    4% |
|       4 |                     800 |           148 |   19% |
|      16 |                     761 |           615 |   81% |
|      32 |                     761 |          1391 |  183% |
|     128 |                     727 |          3657 |  503% |
|     256 |                     759 |          5019 |  661% |
|     512 |                     745 |          6168 |  828% |
|    1024 |                     741 |          6206 |  838% |
| 1048576 |                     745 |          7564 | 1015% |

### 32-bit

|   Bytes | SipHasher (MB/s) | XXHash32 (MB/s) | Ratio |
|---------|------------------|-----------------|-------|
|       1 |               52 |              55 |  106% |
|       4 |              210 |             210 |  100% |
|      16 |              615 |            1230 |  200% |
|      32 |              914 |            1882 |  206% |
|     128 |             1347 |            3282 |  244% |
|     256 |             1414 |            3459 |  245% |
|     512 |             1546 |            3792 |  245% |
|    1024 |             1565 |            3938 |  252% |
| 1048576 |             1592 |            4127 |  259% |

|   Bytes | [FnvHasher][fnv] (MB/s) | XXHash32 (MB/s) | Ratio |
|---------|-------------------------|-----------------|-------|
|       1 |                    1000 |              55 |    6% |
|       4 |                     800 |             210 |   26% |
|      16 |                     761 |            1230 |  162% |
|      32 |                     761 |            1882 |  247% |
|     128 |                     727 |            3282 |  451% |
|     256 |                     759 |            3459 |  456% |
|     512 |                     745 |            3792 |  509% |
|    1024 |                     741 |            3938 |  531% |
| 1048576 |                     745 |            4127 |  554% |


[fnv]: https://github.com/servo/rust-fnv

## Contributing

1. Fork it ( https://github.com/shepmaster/twox-hash/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Add a failing test.
4. Add code to pass the test.
5. Commit your changes (`git commit -am 'Add some feature'`)
6. Ensure tests pass.
7. Push to the branch (`git push origin my-new-feature`)
8. Create a new Pull Request
