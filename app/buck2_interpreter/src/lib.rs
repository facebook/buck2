/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implements Buck's handling of target patterns and parsing of build files.

#![feature(pattern)]
#![feature(try_blocks)]
#![feature(never_type)]
#![feature(box_patterns)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

#[macro_use]
extern crate starlark;

pub mod build_context;
pub mod bxl;
pub mod coerce;
pub mod dice;
pub mod extra;
pub mod factory;
pub mod file_loader;
pub mod file_type;
pub mod functions;
pub mod import_paths;
pub mod load_module;
pub mod package_imports;
pub mod parse_import;
pub mod path;
pub mod prelude_path;
pub mod print_handler;
pub mod starlark_debug;
pub mod starlark_profiler;
pub mod starlark_promise;
pub mod types;
