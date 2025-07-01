/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//!
//! bxl is the Buck Extension Language, allowing any integrator to write Starlark code that
//! introspects buck2 internal graphs in a safe, incremental way to perform more complex operations

pub mod build_result;

pub mod anon_target;
pub mod calculation;
pub mod result;
pub mod select;
pub mod types;
pub mod unconfigured_attribute;
