/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::env_helper::EnvHelper;

pub(crate) mod build_id_writer;
pub mod display;
pub mod event_log;
pub(crate) mod get;
pub(crate) mod humanized_bytes;
pub(crate) mod io;
pub(crate) mod last_command_execution_kind;
pub mod re_log;
pub(crate) mod re_panel;
pub(crate) mod recorder;
pub(crate) mod simpleconsole;
pub(crate) mod span_tracker;
pub(crate) mod stdout_stderr_forwarder;
pub mod subscriber;
pub mod subscriber_unpack;
pub mod superconsole;
pub(crate) mod two_snapshots;

pub fn disable_log_upload() -> anyhow::Result<bool> {
    static DISABLE_LOG_UPLOAD: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_DISABLE_LOG_UPLOAD");
    Ok(DISABLE_LOG_UPLOAD.get()?.copied().unwrap_or_default())
}
