## pico-args
![Build Status](https://github.com/RazrFalcon/pico-args/workflows/Rust/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/pico-args.svg)](https://crates.io/crates/pico-args)
[![Documentation](https://docs.rs/pico-args/badge.svg)](https://docs.rs/pico-args)
[![Rust 1.32+](https://img.shields.io/badge/rust-1.31+-orange.svg)](https://www.rust-lang.org)
![](https://img.shields.io/badge/unsafe-forbidden-brightgreen.svg)

An ultra simple CLI arguments parser.

If you think that this library doesn't support some feature, it's probably intentional.

- No help generation.
- Only flags, options, free arguments and subcommands are supported.
- Options can be separated by a space, `=` or nothing. See build features.
- Arguments can be in any order.
- Non UTF-8 arguments are supported.

### Build features

- `eq-separator`

  Allows parsing arguments separated by `=`. Enabled by default.<br/>
  This feature adds about 1KiB to the resulting binary.

- `short-space-opt`

  Makes the space between short keys and their values optional (e.g. `-w10`).<br/>
  If `eq-separator` is enabled, then it takes precedence and the '=' is not included.<br/>
  If `eq-separator` is disabled, then `-K=value` gives an error instead of returning `"=value"`.<br/>
  The optional space is only applicable for short keys because `--keyvalue` would be ambiguous.

- `combined-flags`

  Allows combination of flags, e.g. `-abc` instead of `-a -b -c`. If `short-space-opt` or `eq-separator` are enabled, you must parse flags after values, to prevent ambiguities.

### Alternatives

The core idea of `pico-args` is to provide some "sugar" for arguments parsing without
a lot of overhead (binary or compilation time wise).
There are no point in comparing parsing features since `pico-args` supports
only the bare minimum. So we will compare only the size overhead and compilation time.

There are a lot of arguments parsing implementations, but we will use only these one:

- [clap](https://crates.io/crates/clap) - is the most popular and complete one
- [gumdrop](https://crates.io/crates/gumdrop) - a simple parser that uses procedural macros
- [structopt](https://crates.io/crates/structopt) - a two above combined
- [argh](https://crates.io/crates/argh) - similar to gumdrop

|                        | null    | `pico-args` | `clap`   | `gumdrop` | `structopt` | `argh`  |
|------------------------|---------|-------------|----------|-----------|-------------|---------|
| Binary overhead        | 0KiB    | **14.3KiB** | 373.0KiB | 19.8KiB   | 371.4KiB    | 17.6KiB |
| Build time             | 0.4s    | **0.7s**    | 5.6s     | 4.1s      | 6.2s        | 4.0s    |
| Number of dependencies | 0       | **0**       | 8        | 5         | 20          | 8       |
| Tested version         | -       | 0.4.0       | 2.33.3   | 0.8.0     | 0.3.21      | 0.1.4   |

- Binary size overhead was measured by subtracting the `.text` section size of an app with
  arguments parsing and a hello world app.
- Build time was measured using `hyperfine 'cargo clean; cargo build --release'`.
- Test projects can be found in `test-apps/`.

### License

MIT
