# Custom targets

This directory contains custom target definitions for buck2 to pass to the rust toolchain.
The simplest way to generate one from an existing target triple is to run

```
RUSTC_BOOTSTRAP=1 rustc --print target-spec-json -Zunstable-options --target <triple>
```

## Targets

- **aarch64-unknown-linux-gnu-unstable**: A copy of `aarch64-unknown-linux-gnu` with unstable target-features enabled. Doing so suppresses the typical "unstable feature specified for `-Ctarget-feature`" warning.
