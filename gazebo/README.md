# Gazebo - a library of Rust utilities

[![Support Ukraine](https://img.shields.io/badge/Support-Ukraine-FFD500?style=flat&labelColor=005BBB)](https://opensource.fb.com/support-ukraine)
[![GitHub link](https://img.shields.io/badge/GitHub-facebookincubator%2Fgazebo-blue.svg)](https://github.com/facebookincubator/gazebo)
[![crates.io version](https://img.shields.io/crates/v/gazebo.svg)](https://crates.io/crates/gazebo)
[![docs.rs availability](https://img.shields.io/docsrs/gazebo?label=docs.rs)](https://docs.rs/gazebo/)
[![Build status](https://img.shields.io/github/workflow/status/facebookincubator/gazebo/ci.svg)](https://github.com/facebookincubator/gazebo/actions)

This library contains a collection of well-tested utilities. Most modules stand alone, but taking a few representative examples:

* `gazebo::prelude::*` is intended to be imported as such, and provides extension traits to common types. For example, it provides `Vec::map` which is equivalent to `iter().map(f).collect::<Vec<_>>()`, and `str::split1` like `split` but which only splits once. We hope some of these functions one day make it into the Rust standard library.
* `gazebo::dupe` provides the trait `Dupe` with the member `dupe`, all of which are exactly like `Clone`. The difference is that `Dupe` should not be implemented for types that reallocate or have expensive `clone` operations - e.g. there is `Dupe` for `Arc` and `usize`, but not for `String` and `Vec`. By using `dupe` it is easy to focus on the `clone` calls (which should be rare) and ignore things whose cost is minimal.
* `gazebo::cell::ARef` provides a type which is either a `Ref<T>` or a direct reference `&T`, with operations that make it look like `Ref` -- allowing you to uniformly convert a reference into something like a `Ref`.
* `gazebo::any::AnyLifetime` provides a trait like `Any`, but which does not require `'static` lifetimes, at the cost of more boilerplate.

The functionality provided by Gazebo is not stable, and continues to evolve with both additions (as we find new useful features) and removals (as we find better patterns or libraries encapsulating the ideas better). While the code varies in usefulness and design quality, it is all well tested and documented.

## Using Gazebo

Gazebo can be depended upon by adding `gazebo` to your `[dependencies]`, using the standard [Cargo patterns](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html).

The two interesting directories in this repo are `gazebo` (which contains the source to Gazebo itself) and `gazebo_derive` (which contains support for `#[derive(Dupe)]` and other Gazebo traits). Usually you will directly import `gazebo`, but `gazebo_derive` is a required transitive dependency if you are sourcing the library from GitHub.

## Learn More

You can learn more about Gazebo in [this introductory video](https://www.youtube.com/watch?v=OsrBYHIYCYk), or from the following blog posts:

* [Rust Nibbles - Gazebo: Prelude](https://developers.facebook.com/blog/post/2021/06/29/rust-nibbles-gazebo-prelude/)
* [Rust Nibbles - Gazebo: Dupe](https://developers.facebook.com/blog/post/2021/07/06/rust-nibbles-gazebo-dupe/)
* [Rust Nibbles - Gazebo: Variants](https://developers.facebook.com/blog/post/2021/07/13/rust-nibbles-gazebo-variants)
* [Rust Nibbles - Gazebo: AnyLifetime](https://developers.facebook.com/blog/post/2021/07/20/rust-nibbles-gazebo-any-lifetime/)
* [Rust Nibbles - Gazebo: Comparisons](https://developers.facebook.com/blog/post/2021/07/27/rust-nibbles-gazebo-comparisons/)
* [Rust Nibbles - Gazebo: Casts and Transmute](https://developers.facebook.com/blog/post/2021/08/03/rust-nibbles-gazebo-casts-transmute/)
* [Rust Nibbles - Gazebo: The rest of the tent](https://developers.facebook.com/blog/post/2021/08/10/rust-nibbles-gazebo-rest-of-tent/)

## Making a release

1. Check the [GitHub Actions](https://github.com/facebookincubator/gazebo/actions) are green.
2. Update `CHANGELOG.md` with the changes since the last release. [This link](https://github.com/facebookincubator/gazebo/compare/v0.1.0...main) can help (update to compare against the last release).
3. Update the version numbers of the two `Cargo.toml` files. Bump them by 0.0.1 if there are no incompatible changes, or 0.1.0 if there are. Bump the dependency in `gazebo` to point at the latest `gazebo_derive` version.
4. Copy the files `CHANGELOG.md`, the two `LICENSE-` files and `README.md` into each `gazebo` and `gazebo_derive` subdirectory.
5. Run `cargo publish --allow-dirty --dry-run`, then without the `--dry-run`, first in `gazebo_derive` and then `gazebo` directories.
6. Create a [GitHub release](https://github.com/facebookincubator/gazebo/releases/new) with `v0.X.Y`, using the `gazebo` version as the name.

## License

Gazebo is both MIT and Apache License, Version 2.0 licensed, as found in the [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) files.
