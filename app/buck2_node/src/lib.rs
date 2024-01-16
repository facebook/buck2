/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(box_patterns)]
#![feature(return_position_impl_trait_in_trait)]

pub mod attrs;
pub mod call_stack;
pub mod cfg_constructor;
pub mod configuration;
pub mod configured_universe;
pub mod load_patterns;
pub mod metadata;
pub mod nodes;
pub mod package;
pub mod package_values_calculation;
pub mod provider_id_set;
pub mod query;
pub mod rule;
pub mod rule_type;
pub mod super_package;
pub mod target_calculation;
pub mod visibility;
