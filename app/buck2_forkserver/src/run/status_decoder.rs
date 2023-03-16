/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::ExitStatus;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_miniperf_proto::MiniperfOutput;

pub enum DecodedStatus {
    /// An actual status.
    Status {
        exit_code: i32,
        execution_stats: Option<buck2_data::CommandExecutionStats>,
    },

    /// Spawn failed, provide the error.
    SpawnFailed(String),
}

#[async_trait]
pub trait StatusDecoder {
    /// Status decoders receive the exit status of the command we ran, but they might also obtain
    /// information out of band to obtain a different exit status.
    async fn decode_status(self, status: ExitStatus) -> anyhow::Result<DecodedStatus>;

    /// Notify this decoder that it will not be used.
    async fn cancel(self) -> anyhow::Result<()>;
}

pub struct DefaultStatusDecoder;

#[async_trait]
impl StatusDecoder for DefaultStatusDecoder {
    async fn decode_status(self, status: ExitStatus) -> anyhow::Result<DecodedStatus> {
        Ok(DecodedStatus::Status {
            exit_code: default_decode_exit_code(status),
            execution_stats: None,
        })
    }

    async fn cancel(self) -> anyhow::Result<()> {
        Ok(())
    }
}

fn default_decode_exit_code(status: ExitStatus) -> i32 {
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

    exit_code.unwrap_or(-1)
}

pub struct MiniperfStatusDecoder {
    out_path: AbsNormPathBuf,
}

impl MiniperfStatusDecoder {
    pub fn new(out_path: AbsNormPathBuf) -> Self {
        Self { out_path }
    }
}

#[async_trait]
impl StatusDecoder for MiniperfStatusDecoder {
    async fn decode_status(self, status: ExitStatus) -> anyhow::Result<DecodedStatus> {
        if !status.success() {
            return Ok(DecodedStatus::Status {
                exit_code: default_decode_exit_code(status),
                execution_stats: None,
            });
        }

        let status = tokio::fs::read(&self.out_path).await.with_context(|| {
            format!(
                "Error reading miniperf output at `{}`",
                self.out_path.display()
            )
        })?;

        tokio::fs::remove_file(&self.out_path)
            .await
            .with_context(|| format!("Error removing miniperf output at `{}`", self.out_path))?;

        let status = bincode::deserialize::<MiniperfOutput>(&status)
            .with_context(|| format!("Invalid miniperf output at `{}`", self.out_path.display()))?;

        match status.raw_exit_code {
            Ok(v) => {
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    let exit_code = default_decode_exit_code(ExitStatus::from_raw(v));
                    Ok(DecodedStatus::Status {
                        exit_code,
                        execution_stats: Some(buck2_data::CommandExecutionStats {
                            cpu_instructions_user: Some(status.user_instructions.adjusted_count()),
                        }),
                    })
                }

                #[cfg(not(unix))]
                {
                    Err(anyhow::anyhow!("Attempted to use Miniperf output off-UNIX"))
                }
            }
            Err(e) => Ok(DecodedStatus::SpawnFailed(e)),
        }
    }

    async fn cancel(self) -> anyhow::Result<()> {
        let res = tokio::fs::remove_file(&self.out_path).await;

        match res {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(anyhow::Error::from(e).context(format!(
                "Error removing miniperf output at `{}`",
                self.out_path
            ))),
        }
    }
}
