/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub(crate) mod anon_target_attr_validation;
pub mod attr;
pub mod attr_type;
pub mod coerced_attr;
pub mod coerced_attr_full;
pub mod coerced_attr_with_type;
pub mod coerced_deps_collector;
pub mod coerced_path;
pub mod coercion_context;
pub mod configurable;
pub mod configuration_context;
pub mod configured_attr;
pub mod configured_attr_full;
pub mod configured_attr_info_for_tests;
pub mod configured_traversal;
pub mod display;
pub mod fmt_context;
pub mod hacks;
pub mod inspect_options;
pub mod json;
pub mod serialize;
pub mod spec;
pub mod testing;
pub mod traversal;
pub mod values;
