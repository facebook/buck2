/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! bxl is the Buck Extension Language, allowing any integrator to write Starlark code that
//! introspects buck2 internal graphs in a safe, incremental way to perform more complex operations
//!

pub mod build_result;

pub mod calculation;
pub mod execution_platform;
pub mod result;

pub use buck2_bxl_core::{BxlFunctionLabel, BxlKey};
