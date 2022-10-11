# yansi-term [![Latest version](https://img.shields.io/crates/v/yansi-term.svg)](https://crates.io/crates/yansi-term) [![Build Status](https://travis-ci.org/botika/yansi-term.svg?branch=master)](https://travis-ci.org/botika/yansi-term)
> Adapted from [`rust-ansi-term`](https://github.com/ogham/rust-ansi-term)

Refactor for use [`fmt::Display`](https://doc.rust-lang.org/std/fmt/trait.Display.html) and `FnOnce(&mut fmt::Formatter) -> fmt::Result` 


This is a library for controlling colours and formatting, such as red bold text or blue underlined text, on ANSI terminals.

### [View the Rustdoc](https://docs.rs/yansi-term)


# Installation

This crate works with [Cargo](http://crates.io). Add the following to your `Cargo.toml` dependencies section:

```toml
[dependencies]
yansi-term = "0.1"
```


## Basic usage

There are two main types in this crate that you need to be concerned with: `Style`, and `Colour`.

A `Style` holds stylistic information: foreground and background colours, whether the text should be bold, or blinking, or other properties.
The `Colour` enum represents the available colours.

`Color` is also available as an alias to `Colour`.

To format a string, call the `paint` method on a `Style` or a `Colour`, passing in the string you want to format as the argument.
For example, here’s how to get some red text:

```rust
use yansi_term::Colour::Red;

println!("This is in red: {}", Red.paint("a red string"));
```

**Note for Windows 10 users:** On Windows 10, the application must enable ANSI support first:

```rust,ignore
let enabled = yansi_term::enable_ansi_support();
```

## Bold, underline, background, and other styles

For anything more complex than plain foreground colour changes, you need to construct `Style` values themselves, rather than beginning with a `Colour`.
You can do this by chaining methods based on a new `Style`, created with `Style::new()`.
Each method creates a new style that has that specific property set.
For example:

```rust
use yansi_term::Style;

println!("How about some {} and {}?",
         Style::new().bold().paint("bold"),
         Style::new().underline().paint("underline"));
```

For brevity, these methods have also been implemented for `Colour` values, so you can give your styles a foreground colour without having to begin with an empty `Style` value:

```rust
use yansi_term::Colour::{Blue, Yellow};

println!("Demonstrating {} and {}!",
         Blue.bold().paint("blue bold"),
         Yellow.underline().paint("yellow underline"));

println!("Yellow on blue: {}", Yellow.on(Blue).paint("wow!"));
```

The complete list of styles you can use are:
`bold`, `dimmed`, `italic`, `underline`, `blink`, `reverse`, `hidden`, and `on` for background colours.

In some cases, you may find it easier to change the foreground on an existing `Style` rather than starting from the appropriate `Colour`.
You can do this using the `fg` method:

```rust
use yansi_term::Style;
use yansi_term::Colour::{Blue, Cyan, Yellow};

println!("Yellow on blue: {}", Style::new().on(Blue).fg(Yellow).paint("yow!"));
println!("Also yellow on blue: {}", Cyan.on(Blue).fg(Yellow).paint("zow!"));
```

You can turn a `Colour` into a `Style` with the `normal` method.

```rust
use yansi_term::Style;
use yansi_term::Colour::Red;

Red.normal().paint("yet another red string");
Style::default().paint("a completely regular string");
```


## Extended colours

You can access the extended range of 256 colours by using the `Colour::Fixed` variant, which takes an argument of the colour number to use.
This can be included wherever you would use a `Colour`:

```rust
use yansi_term::Colour::Fixed;

Fixed(134).paint("A sort of light purple");
Fixed(221).on(Fixed(124)).paint("Mustard in the ketchup");
```

The first sixteen of these values are the same as the normal and bold standard colour variants.
There’s nothing stopping you from using these as `Fixed` colours instead, but there’s nothing to be gained by doing so either.

You can also access full 24-bit colour by using the `Colour::RGB` variant, which takes separate `u8` arguments for red, green, and blue:

```rust
use yansi_term::Colour::RGB;

RGB(70, 130, 180).paint("Steel blue");
```
