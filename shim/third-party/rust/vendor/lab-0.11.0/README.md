# Lab

[![Lab crate](https://img.shields.io/crates/v/lab.svg)](https://crates.io/crates/lab)
[![Lab documentation](https://docs.rs/lab/badge.svg)](https://docs.rs/lab)
![minimum rustc 1.31](https://img.shields.io/badge/rustc-1.36+-red.svg)
[![TravisCI Build Status](https://travis-ci.com/TooManyBees/lab.svg)](https://travis-ci.com/github/TooManyBees/lab)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/TooManyBees/lab?branch=master&svg=true)](https://ci.appveyor.com/project/TooManyBees/lab)

Tools for converting RGB colors to L\*a\*b\* measurements.

RGB colors, for this crate at least, are considered to be composed of `u8`
values from 0 to 255, while L\*a\*b\* colors are represented by its own struct
that uses `f32` values.

# Usage

## Converting single values

To convert a single value, use one of the functions

* `lab::Lab::from_rgb(rgb: &[u8; 3]) -> Lab`
* `lab::Lab::from_rgba(rgba: &[u8; 4]) -> Lab` (drops the fourth alpha byte)
* `lab::Lab::to_rgb(&self) -> [u8; 3]`

```rust
extern crate lab;
use lab::Lab;

let pink_in_lab = Lab::from_rgb(&[253, 120, 138]);
// Lab { l: 66.639084, a: 52.251457, b: 14.860654 }
```

## Converting multiple values

To convert slices of values

* `lab::rgbs_to_labs(rgbs: &[[u8; 3]]) -> Vec<Lab>`
* `lab::labs_to_rgbs(labs: &[Lab]) -> Vec<[u8; 3]>`
* `lab::rgb_bytes_to_labs(bytes: &[u8]) -> Vec<Lab>`
* `lab::labs_to_rgb_bytes(labs: &[Lab]) -> Vec<u8>`

```rust
extern crate lab;
use lab::rgbs_to_labs;

let rgbs = vec![
    [0xFF, 0x69, 0xB6],
    [0xE7, 0x00, 0x00],
    [0xFF, 0x8C, 0x00],
    [0xFF, 0xEF, 0x00],
    [0x00, 0x81, 0x1F],
    [0x00, 0xC1, 0xC1],
    [0x00, 0x44, 0xFF],
    [0x76, 0x00, 0x89],
];

let labs = rgbs_to_labs(&rgbs);
```

```rust
extern crate lab;
use lab::rgb_bytes_to_labs;

let rgbs = vec![
    0xFF, 0x69, 0xB6,
    0xE7, 0x00, 0x00,
    0xFF, 0x8C, 0x00,
    0xFF, 0xEF, 0x00,
    0x00, 0x81, 0x1F,
    0x00, 0xC1, 0xC1,
    0x00, 0x44, 0xFF,
    0x76, 0x00, 0x89,
];

let labs = rgb_bytes_to_labs(&rgbs);
```

These functions will use x86_64 AVX2 instructions if compiled to a supported target.

## Minimum Rust version

Lab 0.8.0 requires Rust >= 1.36.0 for the [MaybeUninit](https://doc.rust-lang.org/std/mem/union.MaybeUninit.html) struct

Lab 0.7.0 requires Rust >= 1.31.0 for the [chunks_exact](https://doc.rust-lang.org/std/primitive.slice.html#method.chunks_exact) slice method
