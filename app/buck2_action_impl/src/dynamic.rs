/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub(crate) mod attrs;
pub mod attrs_starlark;
pub mod bxl;
pub mod calculation;
pub mod deferred;
pub(crate) mod dynamic_actions;
pub mod dynamic_actions_callable;
pub(crate) mod dynamic_actions_globals;
pub(crate) mod dynamic_value;
pub mod params;
pub(crate) mod resolved_dynamic_value;
pub(crate) mod storage;
pub use dynamic_actions_globals::new_dynamic_actions_callable;
