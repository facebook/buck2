---
id: bootstrapping
title: Bootstrapping Buck2
---

# Bootstrapping Buck2

To generate `BUCK` files for `buck2`'s dependencies, we use [reindeer](https://github.com/facebookincubator/reindeer).

Note that the resulting binary will be compiled without optimisations or [jemalloc](https://github.com/jemalloc/jemalloc), so we recommend using the Cargo-produced binary in further development.

First, install `reindeer` with `Cargo`:
```sh
cargo install --git  https://github.com/facebookincubator/reindeer --rev c78947234f5b61d5b8e56831c75790bb326a28c9
```

Next, run the following to pull in dependencies and buckify:
```sh
cd buck2/
reindeer --third-party-dir shim/third-party/rust vendor
reindeer --third-party-dir shim/third-party/rust buckify --stdout > shim/third-party/rust/BUCK
```

Build `buck2` with `buck2`:
```sh
buck2 build //:buck2
```
