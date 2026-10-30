/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Typed buck settings, replaces buckconfig for core buck2 settings.

pub mod agent_config_change;
pub mod agent_interactive;
pub mod agent_materialization;
mod analysis;
pub mod args;
pub mod dice;
mod hydration;
mod log_download;
mod metadata;
mod package_visibility;
pub mod parser;
mod path;
#[cfg(fbcode_build)]
mod rollouts;
pub mod settings;
pub mod telemetry;

pub use settings::BuckSettingChange;
pub use settings::BuckSettings;
pub use settings::DaemonBuckSettings;
pub use settings::InvocationBuckSettings;
pub use settings::PackageVisibilityDefaultIntersection;
pub use settings::PackageVisibilitySection;
