# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.11.1 (2022-08-08)

<csr-id-71977b26194ce6049a063131792760933952424b/>
<csr-id-3068a18e997ef04c302a10b51271353897a92027/>

### Chore

 - <csr-id-71977b26194ce6049a063131792760933952424b/> Update siphasher to 0.3

### Other

 - <csr-id-3068a18e997ef04c302a10b51271353897a92027/> make uncased feature compatible with no_std


### Commit Statistics

<csr-read-only-do-not-edit/>

 - 130 commits contributed to the release over the course of 2946 calendar days.
 - 2 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Replace handmade changelog with generated one by `cargo-smart-release` ([`cb84cf6`](https://github.com/rust-phf/rust-phf/commit/cb84cf6636ab52823c53e70d6abeac8f648a3482))
    - Add README.md for some crates ([`e0b34fa`](https://github.com/rust-phf/rust-phf/commit/e0b34fa0a697f45f2c41a875bf84b78a6d3ce079))
    - Add category to crates ([`32a72c3`](https://github.com/rust-phf/rust-phf/commit/32a72c3859997fd6b590e9ec092ae789d2acdf55))
    - Update repository links on Cargo.toml ([`1af3b0f`](https://github.com/rust-phf/rust-phf/commit/1af3b0fe1f8fdcae7ccc1bc8d51de309fb16a6bf))
    - Release 0.11.0 ([`d2efdc0`](https://github.com/rust-phf/rust-phf/commit/d2efdc08a7eb1d0d6c414b7b2ac41ce1fe1f9a43))
    - Make crates edition 2021 ([`b9d25da`](https://github.com/rust-phf/rust-phf/commit/b9d25da58b912d9927fbc41901631cd77836462b))
    - Make "unicase + macros" features work ([`11bb242`](https://github.com/rust-phf/rust-phf/commit/11bb2426f0237b1ecea8c8038630b1231ede4871))
    - Fix build issues. ([`ee82cc9`](https://github.com/rust-phf/rust-phf/commit/ee82cc994573fbb774a0006f13bceb871710fdb6))
    - Implement PhfHash for arrays of any size. ([`cf0f6ec`](https://github.com/rust-phf/rust-phf/commit/cf0f6ec2870ab8d9c7339fe72a536c526003263f))
    - Refine doc comments ([`d8cfc43`](https://github.com/rust-phf/rust-phf/commit/d8cfc436059a1c2c3ede1afb0f9ec2333c046fc6))
    - make uncased feature compatible with no_std ([`3068a18`](https://github.com/rust-phf/rust-phf/commit/3068a18e997ef04c302a10b51271353897a92027))
    - Fix CI failure ([`d9b5ff2`](https://github.com/rust-phf/rust-phf/commit/d9b5ff23367d2bbcc385ff8243c7d972f45d459c))
    - Prepare for release 0.10.0 ([`588ac25`](https://github.com/rust-phf/rust-phf/commit/588ac25dd5c0afccea084e6f94867328a6a30454))
    - Minor cleanups ([`8868d08`](https://github.com/rust-phf/rust-phf/commit/8868d088e2fed36fcd7741e9a1c5bf68bef4f46e))
    - Prepare 0.9.0 release ([`2ca46c4`](https://github.com/rust-phf/rust-phf/commit/2ca46c4f9c9083c128fcc6add33dc5986638940f))
    - Run rustfmt ([`dd86c6c`](https://github.com/rust-phf/rust-phf/commit/dd86c6c103f25021b52144085b8fab0a94582bef))
    - Fix some clippy warnings ([`9adc370`](https://github.com/rust-phf/rust-phf/commit/9adc370ead7fbcc36cd0c74f495ab7631e0c9754))
    - Cleanup docs ([`ddecc3a`](https://github.com/rust-phf/rust-phf/commit/ddecc3aa97aec6d9e9d6e59c57bc598d476335c1))
    - Add support for uncased ([`2a6087f`](https://github.com/rust-phf/rust-phf/commit/2a6087fcaf99b445ff6013f693f7c4fe5d6f7387))
    - replace uses of `std::borrow::Borrow` with new `PhfBorrow` trait ([`b2f3a9c`](https://github.com/rust-phf/rust-phf/commit/b2f3a9c6a95ebabc2b0ae7ed1ec3ee7d72418e85))
    - Allow using the owned `String` type for `phf` dynamic code generation. ([`58dfc05`](https://github.com/rust-phf/rust-phf/commit/58dfc05a7d27ac506016186970b4f3697c1c6475))
    - Release v0.8.0 ([`4060288`](https://github.com/rust-phf/rust-phf/commit/4060288dc2c1ebe3b0630e4016ed51935bb0c863))
    - switch optional `core` feature to default `std` feature ([`645e23d`](https://github.com/rust-phf/rust-phf/commit/645e23dda30ac1b99af39f201a74211e7ac3251a))
    - use sip128 instead of hashing twice ([`a8e67c3`](https://github.com/rust-phf/rust-phf/commit/a8e67c37486974f810737add6fce73e82aeb39aa))
    - use two separate hashes and full 32-bit displacements ([`9b70bd9`](https://github.com/rust-phf/rust-phf/commit/9b70bd94f8b0b74f156e75ccefbd4a4c7ba29728))
    - Merge pull request #159 from upsuper/rand-07 ([`f6407a0`](https://github.com/rust-phf/rust-phf/commit/f6407a056d432326bbfa42f476736ce754354e3e))
    - fix formatting for arrays after #156 ([`40c1476`](https://github.com/rust-phf/rust-phf/commit/40c147691acd4996fc6883a05734fc6da125143d))
    - fix `FmtConst` for `[u8]` ([`12b4fde`](https://github.com/rust-phf/rust-phf/commit/12b4fde5850f2de79e9cf5163689624a3a9787a1))
    - Merge branch 'master' into patch-1 ([`cd0d7ce`](https://github.com/rust-phf/rust-phf/commit/cd0d7ce1194252dcaca3153988ba2a4effa66b4f))
    - implement support for 128-bit ints and fix high magnitude vals ([`5be5919`](https://github.com/rust-phf/rust-phf/commit/5be59199389c0703fff62f640eb1a0d19243fc48))
    - convert to 2018 edition ([`9ff66ab`](https://github.com/rust-phf/rust-phf/commit/9ff66ab36a23c7170cc775773f042a06de426c3b))
    - upgrade `unicase` ([`4a7f766`](https://github.com/rust-phf/rust-phf/commit/4a7f7667598e7beb4c76c72b2bf2fb6571f6dbd9))
    - don't rely on `fmt::Debug` for codegen ([`97405f5`](https://github.com/rust-phf/rust-phf/commit/97405f5be14738dc5d03a8b309297ffa295f4702))
    - Update siphasher to 0.3 ([`71977b2`](https://github.com/rust-phf/rust-phf/commit/71977b26194ce6049a063131792760933952424b))
    - Release v0.7.24 ([`1287414`](https://github.com/rust-phf/rust-phf/commit/1287414b1302d2d717c5f4be81accf4c12ccad48))
    - Downgrade siphasher ([`54dd1e2`](https://github.com/rust-phf/rust-phf/commit/54dd1e22ccb0788fab5240feb5502e02c7b034b9))
    - Upgrade rand and siphasher ([`80d9894`](https://github.com/rust-phf/rust-phf/commit/80d9894e5db7b5a8acf5b89716ee506de2a95b99))
    - Release v0.7.23 ([`a050b6f`](https://github.com/rust-phf/rust-phf/commit/a050b6f2a6b825bf0824339266ab9545340420d4))
    - Release 0.7.22 ([`ab88405`](https://github.com/rust-phf/rust-phf/commit/ab884054fa17eef915db2bdb5259c7aa71fbfea6))
    - Release v0.7.21 ([`6c7e2d9`](https://github.com/rust-phf/rust-phf/commit/6c7e2d9ce17ff1b87507925bdbe87e6e682ed3e4))
    - Link to docs.rs ([`61142c5`](https://github.com/rust-phf/rust-phf/commit/61142c5aa168cff1bf53a6961ddc12012b49e1bb))
    - Switch to non-deprecated SipHasher ([`fb3c115`](https://github.com/rust-phf/rust-phf/commit/fb3c115a306e6e0a12b88e12d7178857dbc5f66e))
    - Release v0.7.20 ([`f631f50`](https://github.com/rust-phf/rust-phf/commit/f631f50abfaf6ea3d6fc8caaada47975b6df3a62))
    - Release v0.7.19 ([`0a98dd1`](https://github.com/rust-phf/rust-phf/commit/0a98dd1865d12a3fa4cc27bdb38fa1e7374940d9))
    - Release v0.7.18 ([`3f71765`](https://github.com/rust-phf/rust-phf/commit/3f717650f4331f5dbb9d7a3f878228fcf1138729))
    - Release v0.7.17 ([`21ecf72`](https://github.com/rust-phf/rust-phf/commit/21ecf72101715e4754db95a64ecd7de5a37b7f14))
    - Add UniCase support to phf_macros and bump unicase version ([`2af3abb`](https://github.com/rust-phf/rust-phf/commit/2af3abb00cafc85d43755e43767a2a8b274f6670))
    - Release v0.7.16 ([`8bf29c1`](https://github.com/rust-phf/rust-phf/commit/8bf29c10a878c83d73cc40385f0e96cb9cc95afa))
    - Release v0.7.15 ([`20f896e`](https://github.com/rust-phf/rust-phf/commit/20f896e6975cabb9cf9883b08eaa5b3da8597f11))
    - Release v0.7.14 ([`fee66fc`](https://github.com/rust-phf/rust-phf/commit/fee66fc20e33f2b119f830a8926f3b6e52abcf09))
    - Add an impl of PhfHash for UniCase ([`d761144`](https://github.com/rust-phf/rust-phf/commit/d761144daf92ce6aed83165aa840a1ae72bd0bb2))
    - Drop all rust features ([`888f623`](https://github.com/rust-phf/rust-phf/commit/888f6234cd4e26e08b1f2d3716e4d4e0b95d0196))
    - Conditionally compile String and Vec impls ([`8105ae8`](https://github.com/rust-phf/rust-phf/commit/8105ae8f6c1e4fde641716521b327eb07cf648cc))
    - Implement PhfHash for String and Vec<u8> ([`ae820e6`](https://github.com/rust-phf/rust-phf/commit/ae820e6b8c8b4a46083ea4105ec3b378d52e8db0))
    - Release v0.7.13 ([`4769a6d`](https://github.com/rust-phf/rust-phf/commit/4769a6d2ce1d392da06e4b3cb833a1cdccb1f1aa))
    - Release v0.7.12 ([`9b75ee5`](https://github.com/rust-phf/rust-phf/commit/9b75ee5ed14060c45a5785fba0387be09e698624))
    - Release v0.7.11 ([`a004227`](https://github.com/rust-phf/rust-phf/commit/a0042277b181ec95fcbf29751b9a453f4f962ebb))
    - Release v0.7.10 ([`c43154b`](https://github.com/rust-phf/rust-phf/commit/c43154b2661dc09620a7879c16f37b47d6ec03ae))
    - Release v0.7.9 ([`b7d29df`](https://github.com/rust-phf/rust-phf/commit/b7d29dfe0df288b2da74de195f764eace1c8e443))
    - Release v0.7.8 ([`aad0b9b`](https://github.com/rust-phf/rust-phf/commit/aad0b9b658fb970e3df60b066961aafca1a17c44))
    - Release v0.7.7 ([`c9e7a93`](https://github.com/rust-phf/rust-phf/commit/c9e7a93f4d6f85a72651aba6187e4c956d8c1167))
    - Run through rustfmt ([`58e2223`](https://github.com/rust-phf/rust-phf/commit/58e222380b7fc9609a055cb5a6110ba04e47d677))
    - Release v0.7.6 ([`5bcd5c9`](https://github.com/rust-phf/rust-phf/commit/5bcd5c95215f5aa29e133cb2912662085a8158f0))
    - Simplify no_std logic a bit ([`70f2ed9`](https://github.com/rust-phf/rust-phf/commit/70f2ed93d2e64b822bf2a23fde0ee848e8785bd1))
    - Reinstantiate no_std cargo feature flag. ([`7c3f757`](https://github.com/rust-phf/rust-phf/commit/7c3f757cdc83b4035d81f0d521b4b80b9080155e))
    - Release v0.7.5 ([`fda44f5`](https://github.com/rust-phf/rust-phf/commit/fda44f550401c1bd4aad29bb2c07030b86761028))
    - Release v0.7.4 ([`c7c0d3c`](https://github.com/rust-phf/rust-phf/commit/c7c0d3c294126157f0275a05b7c3a65c419234a1))
    - Add hash() and get_index() to phf_shared. ([`d3b2ea0`](https://github.com/rust-phf/rust-phf/commit/d3b2ea0f0a9bd9cb79da90d8795f1905c3df1f5f))
    - Update PhfHash to mirror std::hash::Hash ([`96ef156`](https://github.com/rust-phf/rust-phf/commit/96ef156baae669b233673d6be2b96617ad48551e))
    - Make PhfHash endianness-independent ([`8f406b9`](https://github.com/rust-phf/rust-phf/commit/8f406b910a2ec0f389b977614f8de3151bb17070))
    - Release v0.7.3 ([`77ea239`](https://github.com/rust-phf/rust-phf/commit/77ea23917e908b10c4c5c463671a8409292f8661))
    - Release v0.7.2 ([`642b69d`](https://github.com/rust-phf/rust-phf/commit/642b69d0100a4ee7ec6e430ef1351bd1f28f9a4a))
    - Release v0.7.1 ([`9cb9de9`](https://github.com/rust-phf/rust-phf/commit/9cb9de911ad4e16964f0def29780dde1630c3619))
    - Release v0.7.0 ([`555a690`](https://github.com/rust-phf/rust-phf/commit/555a690561673597aee068650ac884bbcc2e31cf))
    - Release v0.6.19 ([`5810d30`](https://github.com/rust-phf/rust-phf/commit/5810d30ef2162f33cfb4da99c65b7344c7f2913b))
    - Release v0.6.18 ([`36efc72`](https://github.com/rust-phf/rust-phf/commit/36efc721478d097fba1e5458cbdd9f288637abae))
    - Fix for upstream changes ([`eabadcf`](https://github.com/rust-phf/rust-phf/commit/eabadcf7e8af351ba8f07d86746e35adc8c5812e))
    - Release v0.6.17 ([`271ccc2`](https://github.com/rust-phf/rust-phf/commit/271ccc27d885363d4d8c549f75624d08c48e56c5))
    - Release v0.6.15 ([`ede14df`](https://github.com/rust-phf/rust-phf/commit/ede14df1e574674852b09bcafff4ad549ebfd4ae))
    - Release v0.6.14 ([`cf64ebb`](https://github.com/rust-phf/rust-phf/commit/cf64ebb8f769c9f12c9a03d05713dde6b8caf371))
    - Update to rustc 1.0.0-dev (e46610966 2015-03-17) (built 2015-03-17) ([`54f32dd`](https://github.com/rust-phf/rust-phf/commit/54f32dd4cba60fd4833cd2cf0e1030cfd9a9ca4b))
    - Release v0.6.13 ([`4fdb533`](https://github.com/rust-phf/rust-phf/commit/4fdb5331fd9978ca3e180a06fb2e34627f50fb77))
    - Release v0.6.12 ([`59ca586`](https://github.com/rust-phf/rust-phf/commit/59ca58637206c9806c13cc24cb35cb7d0ce9d23f))
    - Release v0.6.11 ([`e1e6d3b`](https://github.com/rust-phf/rust-phf/commit/e1e6d3b40a6babddd0989406f2b4e952443ff52e))
    - Release v0.6.10 ([`fc45373`](https://github.com/rust-phf/rust-phf/commit/fc45373b34a461664f532c5108f3d2625172c128))
    - Add doc URLs ([`4605db3`](https://github.com/rust-phf/rust-phf/commit/4605db3e7e0c4bef09ccf6c09c7dbcc36b707a9f))
    - Remove core feature ([`d4c189a`](https://github.com/rust-phf/rust-phf/commit/d4c189a2b060df33e7c97d6c1f0f430b68fc23b5))
    - Release v0.6.9 ([`822f4e3`](https://github.com/rust-phf/rust-phf/commit/822f4e3fb127dc02d36d802803d71aa5b98bed3c))
    - More fixes ([`0c04b9c`](https://github.com/rust-phf/rust-phf/commit/0c04b9cb2679a63394778a7362ef14441b6c2032))
    - Fix for upstream changes ([`f014882`](https://github.com/rust-phf/rust-phf/commit/f01488236a8e944f1b12b4bc441d55c10fc47aa1))
    - Release v0.6.8 ([`cd637ca`](https://github.com/rust-phf/rust-phf/commit/cd637cafb6d37b1901b6c119a7d26f253e9a288e))
    - Release v0.6.7 ([`bfc36c9`](https://github.com/rust-phf/rust-phf/commit/bfc36c979225f652cdb72f3b1f2a25e77b50ab8c))
    - Fix for upstream changes ([`5ff7040`](https://github.com/rust-phf/rust-phf/commit/5ff70403a1b12c30206b128ac619b31c69e42eb4))
    - Release v0.6.6 ([`b09a174`](https://github.com/rust-phf/rust-phf/commit/b09a174a166c7744c5989bedc6ba68340f6f7fd1))
    - Release v0.6.5 ([`271e784`](https://github.com/rust-phf/rust-phf/commit/271e7848f35b31d6ce9fc9268de173738464bfc8))
    - Fix for upstream changes ([`3db7cef`](https://github.com/rust-phf/rust-phf/commit/3db7cef414e4de28eb6c18938c275a3aafbdafa4))
    - Move docs to this repo and auto build them ([`f8ef160`](https://github.com/rust-phf/rust-phf/commit/f8ef160480e2d4ce72fa7afb6ebce70e45acbc76))
    - Release v0.6.4 ([`6866c1b`](https://github.com/rust-phf/rust-phf/commit/6866c1bf5ad5091bc969f1356884aa86c27458cb))
    - Release v0.6.3 ([`b0c5e3c`](https://github.com/rust-phf/rust-phf/commit/b0c5e3cb69742f81160ea80a3ba1782a0b4e01a2))
    - Release v0.6.2 ([`d9ddf45`](https://github.com/rust-phf/rust-phf/commit/d9ddf45b15ba812b0d3acedffb08e901742e56c4))
    - Link to libstd by default ([`24555b1`](https://github.com/rust-phf/rust-phf/commit/24555b19e6b54656633cc4ceac91864f14c20471))
    - Release v0.6.1 ([`ca0e9f6`](https://github.com/rust-phf/rust-phf/commit/ca0e9f6b9c737f3d11bcad2f4624bb5603a8170e))
    - Fix for stability changes ([`f7fb510`](https://github.com/rust-phf/rust-phf/commit/f7fb510dfe67f11522a2d214bd14d21f910bfd7b))
    - Release v0.6.0 ([`09d6870`](https://github.com/rust-phf/rust-phf/commit/09d687053caf4d321f72907528573b3334fae3c2))
    - Release v0.5.0 ([`8683be2`](https://github.com/rust-phf/rust-phf/commit/8683be260effe5605243ef230bad6154ef4e5e20))
    - Fix deprecation warning ([`d0fa86a`](https://github.com/rust-phf/rust-phf/commit/d0fa86a1f37f118382a3dc4400de158f8d181a2a))
    - Release v0.4.9 ([`28cbe70`](https://github.com/rust-phf/rust-phf/commit/28cbe704e0f96495c2527ad93c5e67315c245908))
    - Fix for upstream changes ([`0b22188`](https://github.com/rust-phf/rust-phf/commit/0b22188f5767a0a125d01ed8b176ce19fef95cad))
    - Release v0.4.8 ([`bb858f1`](https://github.com/rust-phf/rust-phf/commit/bb858f11dd88579d47b0089121f8d551731464ab))
    - Release v0.4.7 ([`d83f551`](https://github.com/rust-phf/rust-phf/commit/d83f551a874a24b2a4308804e7cbca32a1aa2494))
    - Fix for upstream changes ([`c3ae5ac`](https://github.com/rust-phf/rust-phf/commit/c3ae5ac94cfa11404b420d45229c3a0d0d8a4535))
    - Release v0.4.6 ([`360bf81`](https://github.com/rust-phf/rust-phf/commit/360bf81ad3aafced75dc64a49e58a867d5239264))
    - Release v0.4.5 ([`ab4786c`](https://github.com/rust-phf/rust-phf/commit/ab4786c09b55e46658f2a66092caf6a782d056a6))
    - Release v0.4.4 ([`f678635`](https://github.com/rust-phf/rust-phf/commit/f678635378555b7d086014b0466aea12a3ae5701))
    - Fix for upstream changes ([`2b4863f`](https://github.com/rust-phf/rust-phf/commit/2b4863fcb5827d5bd89cc278d2a3052b6b3ee20e))
    - Release v0.4.3 ([`4f5902c`](https://github.com/rust-phf/rust-phf/commit/4f5902c222a81da009bf7955bc96568c73b46b13))
    - Release v0.4.2 ([`69d92b8`](https://github.com/rust-phf/rust-phf/commit/69d92b869fab51a31fda6126003edadd9e832b32))
    - Update to rust master ([`4a0d48d`](https://github.com/rust-phf/rust-phf/commit/4a0d48d165d78d1b3e8f791503e220a032d26d24))
    - Release v0.4.1 ([`0fba837`](https://github.com/rust-phf/rust-phf/commit/0fba8374fd6fb1b10d9d456ae4b1310b00e9d9ca))
    - Make sure we're actually no_std ([`126c6e2`](https://github.com/rust-phf/rust-phf/commit/126c6e26345113bc7492c8ef920ad609b0b25af7))
    - Re-fix str and [u8] hashing when cross compiling ([`a0eb200`](https://github.com/rust-phf/rust-phf/commit/a0eb200d87971555d3d7ce8498404844c860a47f))
    - Release v0.4.0 ([`49dbb36`](https://github.com/rust-phf/rust-phf/commit/49dbb3636621c0436e771a4e0ebfe7342b676616))
    - Fix for upstream changes and drop xxhash ([`fc2539f`](https://github.com/rust-phf/rust-phf/commit/fc2539f7893ef0f833a8c13ec77ba317bd8bf43e))
    - Release v0.3.0 ([`0a80b06`](https://github.com/rust-phf/rust-phf/commit/0a80b06ecde77b33cec8c956c67704613fdd313e))
    - add support for [u8, ..N] keys ([`e26947c`](https://github.com/rust-phf/rust-phf/commit/e26947cc264266bcbc85b8cf5c46b2019d654c72))
    - Bump to 0.2 ([`4546f51`](https://github.com/rust-phf/rust-phf/commit/4546f51fccbd56ddf1214fe232db8926d9f471de))
    - Bump to 0.1.0 ([`43d9a50`](https://github.com/rust-phf/rust-phf/commit/43d9a50e6240716d68dadd9d037f22b2f7df4b58))
    - Merge pull request #31 from jamesrhurst/exactsize ([`d20c311`](https://github.com/rust-phf/rust-phf/commit/d20c311e0e519c0ace07c0d2085d6d35e64a5ba8))
    - Make publishable on crates.io ([`4ad2bb2`](https://github.com/rust-phf/rust-phf/commit/4ad2bb27be35015b3f37ec7025c46df9170b3ef9))
    - Pull shared code into a module ([`19c4f8d`](https://github.com/rust-phf/rust-phf/commit/19c4f8d420d3a9ff8e3ace0256198f5db9fccae0))
</details>

