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
#![feature(never_type)]
#![feature(pattern)]
#![feature(result_into_ok_or_err)]

#[cfg(test)]
#[macro_use]
extern crate maplit;

pub mod convert;
pub mod dice;
#[cfg(all(unix, any(feature = "eden_io", feature = "eden_materializer")))]
pub mod eden;
pub mod external_symlink;
pub mod file_ops;
pub mod find_buildfile;
pub mod io;
pub mod legacy_configs;
pub mod memory;
pub mod package_boundary;
pub mod package_listing;
pub mod pattern;
mod sorted_hash_map;
pub mod target_aliases;
pub mod truncate;
