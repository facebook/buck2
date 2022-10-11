# aHash

AHash is a high speed keyed hashing algorithm intended for use in in-memory hashmaps. It provides a high quality 64 bit hash.
AHash is designed for performance and is *not cryptographically secure*.

When it is available aHash takes advantage of the [hardware AES instruction](https://en.wikipedia.org/wiki/AES_instruction_set)
on X86 processors. If it is not available it falls back on a lower quality (but still DOS resistant) [algorithm based on
multiplication](https://github.com/tkaitchuck/aHash/wiki/AHash-fallback-algorithm).

Similar to Sip_hash, aHash is a keyed hash, so two instances initialized with different keys will produce completely different
hashes and the resulting hashes cannot be predicted without knowing the keys.
This prevents DOS attacks where an attacker sends a large number of items whose hashes collide that get used as keys in a hashmap.

# Goals

AHash is intended to be the fastest DOS resistant hash for use in HashMaps available in the Rust language.
Failing in any of these criteria will be treated as a bug.

# Non-Goals

AHash is not:

* A cryptographically secure hash
* Intended to be a MAC
* Intended for network or persisted use

Different computers using aHash will arrive at different hashes for the same input. Similarly the same computer running
different versions of the code may hash the same input to different values.

## Speed

When it is available aHash uses two rounds of AES encryption using the AES-NI instruction per 16 bytes of input.
On an intel i7-6700 this is as fast as a 64 bit multiplication, but it has the advantages of being a much stronger
permutation and handles 16 bytes at a time. This is obviously much faster than most standard approaches to hashing,
and does a better job of scrambling data than most non-secure hashes.

On an intel i7-6700 compiled with flags `-C opt-level=3 -C target-cpu=native -C codegen-units=1`:

| Input   | SipHash 1-3 time | FnvHash time|FxHash time| aHash time| aHash Fallback* |
|----------------|-----------|-----------|-----------|-----------|---------------|
| u8             | 9.3271 ns | 0.808 ns  | **0.594** ns  | 1.5601 ns | 0.688 ns |
| u16            | 9.5139 ns | 0.803 ns  | **0.594** ns  | 1.5643 ns | 0.681 ns |
| u32            | 9.1196 ns | 1.4424 ns | **0.594** ns  | 1.4938 ns | 0.727 ns |
| u64            | 10.854 ns | 3.0484 ns | **0.628** ns  | 1.4793 ns | 0.814 ns |
| u128           | 12.465 ns | 7.0728 ns | **0.799** ns  | 1.4631 ns | 1.3713 ns |
| 1 byte string  | 11.745 ns | 2.4743 ns | 2.4000 ns | 2.3012 ns | **2.1152** ns |
| 3 byte string  | 12.066 ns | 3.5221 ns | 2.9253 ns | 2.4415 ns | **2.1171** ns |
| 4 byte string  | 11.634 ns | 4.0770 ns | **1.8818** ns | 2.3447 ns | 2.1495 ns |
| 7 byte string  | 14.762 ns | 5.9780 ns | 3.2282 ns | 2.3406 ns | **2.1212** ns |
| 8 byte string  | 13.442 ns | 4.0535 ns | 2.9422 ns | 2.3427 ns | **2.1310** ns |
| 15 byte string | 16.880 ns | 8.3434 ns | 4.6070 ns | 2.1650 ns | **2.0424** ns |
| 16 byte string | 15.155 ns | 7.5796 ns | 3.2619 ns | 2.7104 ns | **2.0708** ns |
| 24 byte string | 16.521 ns | 12.492 ns | 3.5424 ns | **2.3514** ns | 4.5853 ns |
| 68 byte string | 24.598 ns | 50.715 ns | 5.8312 ns | **5.4330** ns | 6.9370 ns |
| 132 byte string| 39.224 ns | 119.96 ns | 11.777 ns | **6.3275** ns | 11.739 ns |
|1024 byte string| 254.00 ns | 1087.3 ns | 156.41 ns | **21.341** ns | 75.133 ns |

* Fallback refers to the algorithm aHash would use if AES instruction are unavailable.
For reference a hash that does nothing (not even reads the input data takes) **0.520 ns**. So that represents the fastest
possible time.

As you can see above aHash like FxHash provides a large speedup over SipHash-1-3 which is already nearly twice as fast as SipHash-2-4.

Rust by default uses SipHash-1-3 because faster hash functions such as FxHash are predictable and vulnerable to denial of
service attacks. While aHash has both very strong scrambling as well as very high performance.

AHash performs well when dealing with large inputs because aHash reads 8 or 16 bytes at a time. (depending on availability of AES-NI)

Because of this, and it's optimized logic aHash is able to outperform FxHash with strings.
It also provides especially good performance dealing with unaligned input.
(Notice the big performance gaps between 3 vs 4, 7 vs 8 and 15 vs 16 in FxHash above)

For more a more representative performance comparison which includes the overhead of using a HashMap, see [HashBrown's benchmarks](https://github.com/rust-lang/hashbrown#performance) as HashBrown now uses aHash as it's hasher by default.

## Security

AHash is designed to [prevent an adversary that does not know the key from being able to create hash collisions or partial collisions.](https://github.com/tkaitchuck/aHash/wiki/Attacking-aHash-or-why-it's-good-enough-for-a-hashmap)

This achieved by ensuring that:

* It obeys the '[strict avalanche criterion](https://en.wikipedia.org/wiki/Avalanche_effect#Strict_avalanche_criterion)':
Each bit of input can has the potential to flip every bit of the output.
* Similarly each bit in the key can affect every bit in the output.
* Input bits never affect just one or a very few bits in intermediate state. This is specifically designed to prevent [differential attacks aimed to cancel previous input](https://emboss.github.io/blog/2012/12/14/breaking-murmur-hash-flooding-dos-reloaded/)
* The update function is not 'lossy'. IE: It is composed of reversible operations (such as AES) which means any entropy from the input is not lost.

AHash is not designed to prevent finding the key by observing the output. It is not intended to prevent impossible differential
analysis from finding they key. Instead the security model is to not allow the hashes be made visible. This is not a major
issue for hashMaps because they aren't normally even stored. In practice this means using unique keys for each map
(RandomState does this for you by default), and not exposing the iteration order of long lived maps that an attacker could
conceivably insert elements into. (This is generally recommended anyway, regardless of hash function,
[because even without knowledge of the hash function an attack is possible](https://accidentallyquadratic.tumblr.com/post/153545455987/rust-hash-iteration-reinsertion).)

## Hash quality

It is a high quality hash that produces results that look highly random.
There should be around the same number of collisions for a small number of buckets that would be expected with random numbers.

There are no full 64 bit collisions with smaller than 64 bits of input. Notably this means the hashes are distinguishable from random data.

### aHash is not cryptographically secure

AHash should not be used for situations where cryptographic security is needed.
It is not intended for this and will likely fail to hold up for several reasons.

1. It has not been analyzed for vulnerabilities and may leak bits of the key in its output.
2. It only uses 2 rounds of AES as opposed to the standard of 10. This likely makes it possible to guess the key by observing a large number of hashes.
3. Like any cypher based hash, it will show certain statistical deviations from truly random output when comparing a (VERY) large number of hashes.

There are several efforts to build a secure hash function that uses AES-NI for acceleration, but aHash is not one of them.

## Compatibility

New versions of aHash may change the algorithm slightly resulting in the new version producing different hashes than
the old version even with the same keys. Additionally aHash does not guarantee that it won't produce different
hash values for the same data on different machines, or even on the same machine when recompiled.

For this reason aHash is not recommended for cases where hashes need to be persisted.

## Accelerated CPUs

Hardware AES instructions are built into Intel processors built after 2010 and AMD processors after 2012.
It is also available on [many other CPUs](https://en.wikipedia.org/wiki/AES_instruction_set) should in eventually
be able to get aHash to work. However only X86 and X86-64 are the only supported architectures at the moment, as currently
they are the only architectures for which Rust provides an intrinsic.

# Why use aHash over X

## SipHash

For a hashmap: Because aHash is faster.

SipHash is however useful in other contexts, such as for a HMAC, where aHash would be completely inapproaprate.

*SipHash-2-4* is designed to provide DOS attack resistance, and has no presently known attacks
against this claim that don't involve learning bits of the key.

SipHash is also available in the "1-3" variant which is about twice as fast as the standard version.
The SipHash authors don't recommend using this variation when DOS attacks are a concern, but there are still no known
practical DOS attacks against the algorithm. Rust has opted for the "1-3" version as the  default in `std::collections::HashMap`,
because the speed trade off of "2-4" was not worth it.

As you can see in the table above, aHash is **much** faster than even *SipHash-1-3*, but it also provides DOS resistance,
and any attack against the accelerated form would likely involve a weakness in AES.

## FxHash

In terms of performance, aHash is faster the FXhash for strings and byte arrays but not primitives.
So it might seem like using Fxhash for hashmaps when the key is a primitive is a good idea. This is *not* the case.

When FX hash is operating on a 4 or 8 bite input such as a u32 or a u64, it reduces to multiplying the input by a fixed
constant. This is a bad hashing algorithm because it means that lower bits can never be influenced by any higher bit. In
the context of a hashmap where the low order bits are being used to determine which bucket to put an item in, this isn't
any better than the identity function. Any keys that happen to end in the same bit pattern will all collide. Some examples of where this is likely to occur are:

* Strings encoded in base64
* Null terminated strings (when working with C code)
* Integers that have the lower bits as zeros. (IE any multiple of small power of 2, which isn't a rare pattern in computer programs.)  
  * For example when taking lengths of data or locations in data it is common for values to
have a multiple of 1024, if these were used as keys in a map they will collide and end up in the same bucket.

Like any non-keyed hash FxHash can be attacked. But FxHash is so prone to this that you may find yourself doing it accidentally.

For example it is possible to [accidentally introduce quadratic behavior by reading from one map in iteration order and writing to another.](https://accidentallyquadratic.tumblr.com/post/153545455987/rust-hash-iteration-reinsertion)

Fxhash flaws make sense when you understand it for what it is. It is a quick and dirty hash, nothing more.
it was not published and promoted by its creator, it was **found**!

Because it is error-prone, FxHash should never be used as a default. In specialized instances where the keys are understood
it makes sense, but given that aHash is faster on almost any object, it's probably not worth it.

## FnvHash

FnvHash is also a poor default. It only handles one byte at a time, so it's performance really suffers with large inputs.
It is also non-keyed so it is still subject to DOS attacks and [accidentally quadratic behavior.](https://accidentallyquadratic.tumblr.com/post/153545455987/rust-hash-iteration-reinsertion)

## MurmurHash, CityHash, MetroHash, FarmHash, HighwayHash, XXHash, SeaHash

Murmur, City, Metro, Farm and Highway are all related, and appear to directly replace one another. Sea and XX are independent
and compete.

They are all fine hashing algorithms, they do a good job of scrambling data, but they are all targeted at a different
usecase. They are intended to work in distributed systems where the hash is expected to be the same over time and from one
computer to the next, efficiently hashing large volumes of data.

This is quite different from the needs of a Hasher used in a hashmap. In a map the typical value is under 10 bytes. None
of these algorithms scale down to handle that small of data at a competitive time. What's more the restriction that they
provide consistent output prevents them from taking advantage of different hardware capabilities on different CPUs. It makes
since for a hashmap to work differently on a phone than on a server, or in wasm.

If you need to persist or transmit a hash of a file, then using one of these is probably a good idea. HighwayHash seems to the the preferred solution du jour. But inside a simple Hashmap, stick with aHash.

## AquaHash

AquaHash is structured very similarly to aHash. (Though the two were designed completely independently) It scales up
better and will start outperforming aHash with inputs larger than 5-10KB. However it does not scale down nearly as well and
does poorly with for example a single `i32` as input. It's only implementation at this point is in C++.

## t1ha

T1ha is fast at large sizes and the output is of high quality, but it is not clear what usecase it aims for.
It has many different versions and is very complex, and uses hardware tricks, so one might infer it is meant for
hashmaps like aHash. But any hash using it take at least **20ns**, and it doesn't outperform even SipHash until the
input sizes are larger than 128 bytes. So uses are likely niche.

# License

Licensed under either of:

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
