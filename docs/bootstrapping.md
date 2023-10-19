---
id: bootstrapping
title: Bootstrapping Buck2
---

# Bootstrapping Buck2

To generate `BUCK` files for `buck2`'s dependencies, we use
[reindeer](https://github.com/facebookincubator/reindeer).

Note that the resulting binary will be compiled without optimisations or
[jemalloc](https://github.com/jemalloc/jemalloc), so we recommend using the
Cargo-produced binary in further development.

First, install `reindeer` with `Cargo`:

```sh
cargo install --locked --git https://github.com/facebookincubator/reindeer reindeer
```

Next, run the following to buckify dependencies:

```sh
cd buck2/
reindeer --third-party-dir shim/third-party/rust buckify
```

Build `buck2` with `buck2`:

```sh
buck2 build //:buck2
```
