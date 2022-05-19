/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Common core components of buck2

// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
#![feature(box_syntax)]
#![feature(io_error_more)]
#![feature(is_sorted)]

#[cfg(test)]
#[macro_use]
extern crate maplit;

pub mod dice;
pub mod file_ops;
pub mod io;
pub mod legacy_configs;
pub mod memory;
pub mod package_boundary;
mod sorted_hash_map;
pub mod target_aliases;
pub mod truncate;
