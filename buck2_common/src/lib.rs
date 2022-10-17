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
#![feature(backtrace)]
#![feature(box_syntax)]
#![feature(fs_try_exists)]
#![feature(io_error_more)]
#![feature(is_sorted)]
#![feature(never_type)]
#![feature(pattern)]
#![feature(result_into_ok_or_err)]

#[cfg(test)]
#[macro_use]
extern crate maplit;

pub mod buckd_connection;
pub mod client_utils;
pub mod convert;
pub mod daemon_dir;
pub mod dice;
#[cfg(any(fbcode_build, cargo_internal_build))]
pub mod eden;
pub mod events;
pub mod executor_config;
pub mod external_symlink;
pub mod file_ops;
pub mod find_buildfile;
pub mod home_buck_tmp;
pub mod invocation_paths;
pub mod invocation_roots;
pub mod io;
pub mod legacy_configs;
pub mod liveliness_manager;
pub mod memory;
pub mod package_boundary;
pub mod package_listing;
pub mod pattern;
pub mod result;
pub mod sqlite;
pub mod target_aliases;
pub mod temp_path;
