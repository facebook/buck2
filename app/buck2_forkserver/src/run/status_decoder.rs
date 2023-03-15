/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::ExitStatus;

use async_trait::async_trait;

pub enum DecodedStatus {
    /// An actual status.
    Status(i32),

    /// Spawn failed, provide the error.
    SpawnFailed(String),
}

#[async_trait]
pub trait StatusDecoder {
    /// Status decoders receive the exit status of the command we ran, but they might also obtain
    /// information out of band to obtain a different exit status.
    async fn decode_status(self, status: ExitStatus) -> anyhow::Result<DecodedStatus>;
}

pub struct DefaultStatusDecoder;

#[async_trait]
impl StatusDecoder for DefaultStatusDecoder {
    async fn decode_status(self, status: ExitStatus) -> anyhow::Result<DecodedStatus> {
        Ok(default_decode(status))
    }
}

fn default_decode(status: ExitStatus) -> DecodedStatus {
    let exit_code;

    #[cfg(unix)]
    {
        // Shell convention on UNIX is to return 128 + signal number on a signal exit,
        // so we emulate this here.
        use std::os::unix::process::ExitStatusExt;
        exit_code = status.code().or_else(|| Some(128 + status.signal()?));
    }

    #[cfg(not(unix))]
    {
        exit_code = status.code();
    }

    DecodedStatus::Status(exit_code.unwrap_or(-1))
}
