/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implements Buck's handling of target patterns and parsing of build files.

#![feature(box_syntax)]
#![feature(pattern)]
#![feature(try_blocks)]
#![feature(never_type)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

#[macro_use]
extern crate async_trait;
#[macro_use]
extern crate starlark;

#[cfg(test)]
mod tests;

pub mod build_defs;
pub mod common;
pub mod dice;
pub mod extra;
pub mod file_loader;
pub mod functions;
pub mod globspec;
pub mod import_paths;
pub mod interpreter;
pub mod package_imports;
pub mod parse_import;
pub mod selector;
pub mod starlark_profiler;
pub mod types;
