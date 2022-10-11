# difference.rs [![](https://travis-ci.org/johannhof/difference.rs.svg?branch=master)](https://travis-ci.org/johannhof/difference.rs) [![](https://ci.appveyor.com/api/projects/status/n1nqaitd5uja8tsi/branch/master?svg=true)](https://ci.appveyor.com/project/johannhof/liquid-rust/branch/master) [![](https://coveralls.io/repos/johannhof/difference.rs/badge.svg?branch=master&service=github)](https://coveralls.io/github/johannhof/difference.rs?branch=master) [![](https://img.shields.io/crates/v/difference.svg)](https://crates.io/crates/difference)
A Rust text diffing library with built-in diffing assertion.

__[Documentation](https://johannhof.github.io/difference.rs)__

__[Examples](/Examples.md)__

```rust
use difference::Changeset;

let changeset = Changeset::new("test", "tent", "");

assert_eq!(changeset.diffs, vec![
  Difference::Same("te".to_string()),
  Difference::Rem("s".to_string()),
  Difference::Add("n".to_string()),
  Difference::Same("t".to_string())
]);
```

![](https://raw.githubusercontent.com/johannhof/difference.rs/master/assets/fox.png)
![](https://raw.githubusercontent.com/johannhof/difference.rs/master/assets/github-style.png)

Usage
----------

Add the following to your Cargo.toml:

```toml
[dependencies]
difference = "2.0"
```

Now you can use the crate in your code
```rust
extern crate difference;
```

Using the binary
-----------------

difference can also be used as a command-line application. The best way to install it is using:

```sh
$ cargo install --features=bin
```
