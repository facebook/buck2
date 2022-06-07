/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Because Buck build uses different version of Rust than Cargo build.
#![allow(stable_features)]
#![feature(backtrace)]
#![feature(const_fn_fn_ptr_basics)]
#![feature(const_fn_trait_bound)]
#![feature(const_panic)]
#![feature(control_flow_enum)]
#![feature(map_first_last)]
#![feature(termination_trait_lib)]
#![feature(try_trait_v2)]
#![feature(type_alias_impl_trait)]
#![feature(box_syntax)]
#![feature(never_type)]
#![feature(path_try_exists)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

#[macro_use]
extern crate maplit;

#[macro_use]
pub mod error;

mod ascii_char_set;
pub mod async_once_cell;
pub mod category;
pub mod cells;
pub mod configuration;
pub mod directory;
pub mod env_helper;
pub mod exit_result;
pub mod fs;
pub mod package;
pub mod provider;
pub mod result;
pub mod target;

/// Marker for things that are only sensible to use inside Facebook,
/// not intended to be complete, but intended to be useful to audit
/// en-mass at some point in the future.
pub fn facebook_only() {}
