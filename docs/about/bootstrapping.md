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

## File descriptor limits

Note that on MacOS, the default file descriptor limit is far too small. If you encounter
"Too many open files (os error 24)" errors, do this:

```sh
buck2 kill
ulimit -n unlimited
```

And try again. [This PR](https://github.com/facebook/buck2/pull/928) should address the issue
in Buck2 itself, once completed.