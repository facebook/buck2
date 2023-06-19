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
pub mod event_log;
pub mod get;
pub(crate) mod observer;
pub mod re_log;
pub mod recorder;
pub(crate) mod simpleconsole;
pub mod stdout_stderr_forwarder;
pub mod subscriber;
pub mod subscriber_unpack;
pub mod superconsole;

pub fn should_upload_log() -> anyhow::Result<bool> {
    if buck2_core::is_open_source() {
        return Ok(false);
    }
    static DISABLE_LOG_UPLOAD: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_DISABLE_LOG_UPLOAD");
    Ok(!DISABLE_LOG_UPLOAD.get()?.copied().unwrap_or_default())
}
