[Sequence Trie][doc]
====

[![Build Status](https://travis-ci.org/michaelsproul/rust_sequence_trie.svg?branch=master)](https://travis-ci.org/michaelsproul/rust_sequence_trie)

This is a generic Trie implementation that uses a hash map to store child nodes. The Trie is keyed by lists of type `K`, which can be anything implementing `PartialEq`, `Eq`, `Hash` and `Clone`. If your keys are explicit lists and you want to be able to store a different value for each element of a key, this might be the data structure for you!

For more information, see the [API documentation][doc].

[doc]: https://docs.rs/sequence_trie/

# Usage

Add `sequence_trie` to your `Cargo.toml`.

```toml
[dependencies]
sequence_trie = "*"
```

# See Also
* [Radix Trie][radix-trie] – a trie operating on byte-strings, with better performance and a less ergonomic API.

[radix-trie]: https://github.com/michaelsproul/rust_radix_trie

# License

Licensed under either of:

 * [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)
 * [MIT license](http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual licensed as above, without any
additional terms or conditions.
