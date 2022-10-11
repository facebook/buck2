/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "str_pattern_extensions", feature(pattern))]
#![cfg_attr(feature = "str_pattern_extensions", feature(associated_type_bounds))]

//! A collection of well-tested primitives that have been useful. Most modules stand alone.

pub mod any;
pub mod cast;
pub mod cell;
pub mod cmp;
pub mod coerce;
pub mod display;
pub mod dupe;
pub(crate) mod ext;
pub mod file;
pub mod hash;
pub(crate) mod indenter;
pub mod phantom;
pub mod prelude;
pub mod types;
pub mod variants;

#[cfg(test)]
mod test;

/// Causes Rust to exit the process when any panic occurs.
/// An alternative is to compile your binary _and all dependencies_
/// with `-Cpanic=abort`, which will provide similar behavior and smaller libraries.
pub fn terminate_on_panic() {
    let orig_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        orig_hook(panic_info);
        std::process::exit(1);
    }));
}
