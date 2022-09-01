# Distributed Incremental Computation Engine, aka. DICE

[![Support Ukraine](https://img.shields.io/badge/Support-Ukraine-FFD500?style=flat&labelColor=005BBB)](https://opensource.fb.com/support-ukraine)
[![GitHub link](https://img.shields.io/badge/GitHub-facebookincubator%2Fdice-blue.svg)](https://github.com/facebookincubator/dice)
[![crates.io version](https://img.shields.io/crates/v/dice.svg)](https://crates.io/crates/dice)
[![docs.rs availability](https://img.shields.io/docsrs/dice?label=docs.rs)](https://docs.rs/dice/)
[![Build status](https://img.shields.io/github/workflow/status/facebookincubator/dice/ci.svg)](https://github.com/facebookincubator/dice/actions)

DICE is a distributed incremental computation engine. The distributed part is TODO, but it is an implemented
incremental computation engine that supports parallel computation, and multi-tenancy.

# Documentation
For detailed documentation, see the docs in [dice/docs/index.md](dice/docs/index.md)

## Making a release

1. Check the [GitHub Actions](https://github.com/facebookincubator/dice/actions) are green.
2. Update `CHANGELOG.md` with the changes since the last release. [This link](https://github.com/facebookincubator/dice/compare/v0.1.0...main) can help (update to compare against the last release).
3. Update the version numbers of the two `Cargo.toml` files. Bump them by 0.0.1 if there are no incompatible changes, or 0.1.0 if there are. Bump the dependency in `dice_examples` to point at the latest `dice` version.
4. Copy the files `CHANGELOG.md`, the two `LICENSE-` files and `README.md` into `dice` subdirectory.
5. Run `cargo publish --allow-dirty --dry-run`, then without the `--dry-run` in `dice`. We do not publich `dice_examples`
6. Create a [GitHub release](https://github.com/facebookincubator/dice/releases/new) with `v0.X.Y`, using the `dice` version as the name.

## License

DICE is both MIT and Apache License, Version 2.0 licensed, as found in the [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) files.
