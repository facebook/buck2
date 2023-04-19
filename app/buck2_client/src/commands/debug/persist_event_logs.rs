/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;
use std::process::Stdio;

use anyhow::Context;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::find_certs;
use buck2_client_ctx::manifold;
use buck2_client_ctx::tokio_runtime_setup::client_tokio_runtime;
use buck2_core::env_helper::EnvHelper;
use clap::ArgMatches;
use tokio::fs::File;
use tokio::fs::OpenOptions;
use tokio::io;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncSeekExt;
use tokio::io::AsyncWriteExt;

static UPLOAD_CHUNK_SIZE: EnvHelper<u64> = EnvHelper::new("BUCK2_TEST_MANIFOLD_CHUNK_BYTES");
static MANIFOLD_TTL_S: EnvHelper<u64> = EnvHelper::new("BUCK2_TEST_MANIFOLD_TTL_S");

type MyJoinHandle = Option<tokio::task::JoinHandle<Result<(), anyhow::Error>>>;

#[derive(Debug, clap::Parser)]
#[clap(about = "Command to write event logs to disk and upload them to Manifold in real time")]
pub struct PersistEventLogsCommand {
    #[clap(long, help = "Name this log will take in Manifold")]
    manifold_name: String,
    #[clap(long, help = "Where to write this log to on disk")]
    local_path: String,
    #[clap(long, help = "If present, only write to disk and don't upload")]
    no_upload: bool,
}

impl PersistEventLogsCommand {
    pub fn exec(self, _matches: &ArgMatches, mut ctx: ClientCommandContext<'_>) -> ExitResult {
        buck2_core::facebook_only();
        let runtime = client_tokio_runtime()?;
        let mut stdin = io::BufReader::new(ctx.stdin());
        runtime
            .block_on(self.write(&mut stdin))
            .context("Error writing")?;
        ExitResult::success()
    }

    async fn write(self, stdin: impl io::AsyncBufRead + Unpin) -> anyhow::Result<()> {
        let upload_chunk_size = UPLOAD_CHUNK_SIZE.get_copied()?.unwrap_or(8 * 1024 * 1024);
        let mut should_upload = !self.no_upload;
        let mut file = self.create_log_file().await?;
        let mut read_position: u64 = 0;
        let mut first_upload = true;
        let manifold_path = format!("flat/{}", self.manifold_name);
        poll_and_upload_loop(
            stdin,
            &mut file,
            &mut read_position,
            &mut first_upload,
            &manifold_path,
            upload_chunk_size,
            &mut should_upload,
        )
        .await?;
        if should_upload {
            upload_remaining(
                &mut file,
                read_position,
                first_upload,
                &manifold_path,
                upload_chunk_size,
            )
            .await?;
        };
        Ok(())
    }

    async fn create_log_file(&self) -> Result<tokio::fs::File, anyhow::Error> {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .read(true)
            .open(&self.local_path)
            .await
            .with_context(|| {
                format!(
                    "Failed to open event log for writing at `{}`",
                    self.local_path
                )
            })?;
        Ok(file)
    }
}

async fn poll_and_upload_loop(
    mut stdin: impl io::AsyncBufRead + Unpin,
    file: &mut File,
    read_position: &mut u64,
    first_upload: &mut bool,
    manifold_path: &str,
    chunk_size: u64,
    should_upload: &mut bool,
) -> Result<(), anyhow::Error> {
    let mut write_position: u64 = 0;
    let mut upload_handle: MyJoinHandle = None;
    loop {
        let mut buf = vec![0; 64 * 1024]; // maximum pipe size in linux
        let bytes_read = stdin.read(&mut buf).await?;
        if bytes_read == 0 {
            break; // closed stdin
        }
        write_to_file(file, write_position, &buf[..bytes_read]).await?;
        write_position += bytes_read as u64;
        let cert = find_certs::find_tls_cert()?;
        if *should_upload && upload_handle.as_ref().map_or(true, |h| h.is_finished()) {
            if let Some(upload_handle) = upload_handle.as_mut() {
                // It's finished already, it shouldn't wait much in this await
                if let Err(e) = upload_handle.await.context("Error while uploading chunk")? {
                    let _res = buck2_core::soft_error!("manifold_upload_chunk_error", e);
                    *should_upload = false;
                };
            };
            if write_position - *read_position > chunk_size {
                upload_chunk(
                    file,
                    read_position,
                    chunk_size,
                    manifold_path,
                    first_upload,
                    &mut upload_handle,
                    cert,
                )
                .await?;
            }
        }
    }
    if let Some(handle) = upload_handle {
        handle.await.context("Error while uploading chunk")??;
    };
    Ok(())
}

async fn upload_chunk(
    file: &mut File,
    read_position: &mut u64,
    chunk_size: u64,
    manifold_path: &str,
    first_upload: &mut bool,
    upload_handle: &mut Option<tokio::task::JoinHandle<Result<(), anyhow::Error>>>,
    cert: OsString,
) -> Result<(), anyhow::Error> {
    file.seek(io::SeekFrom::Start(*read_position))
        .await
        .context("Failed to seek log file")?;
    let (buf, len) = read_chunk(file, chunk_size).await?;
    let manifold_path = manifold_path.to_owned();
    if *first_upload {
        *upload_handle = Some(tokio::spawn(async move {
            if let Err(e) = upload_write(manifold_path, &buf).await {
                let _res = buck2_core::soft_error!("manifold_write_error", e);
            }
            Ok(())
        }));
        *first_upload = false;
    } else {
        let read_position_clone = *read_position;
        *upload_handle = Some(tokio::spawn(async move {
            if let Err(e) = upload_append(manifold_path, &buf, read_position_clone, &cert).await {
                let _res = buck2_core::soft_error!("manifold_append_error", e);
            }
            Ok(())
        }));
    }
    *read_position += len as u64;
    Ok(())
}

async fn upload_remaining(
    file: &mut File,
    mut read_position: u64,
    mut first_upload: bool,
    manifold_path: &str,
    chunk_size: u64,
) -> Result<(), anyhow::Error> {
    file.seek(io::SeekFrom::Start(read_position))
        .await
        .context("Failed to seek log file")?;
    let cert = find_certs::find_tls_cert()?;
    loop {
        // Upload in chunks size UPLOAD_CHUNK_SIZE
        let (buf, len) = read_chunk(file, chunk_size).await?;
        if len == 0 {
            break;
        }
        if first_upload {
            upload_write(manifold_path.to_owned(), &buf).await?;
            first_upload = false;
        } else {
            upload_append(manifold_path.to_owned(), &buf, read_position, &cert).await?;
        }
        read_position += len as u64;
    }
    Ok(())
}

async fn read_chunk(
    read_ref: &mut File,
    chunk_size: u64,
) -> Result<(Vec<u8>, usize), anyhow::Error> {
    let mut buf = vec![];
    let mut handle = read_ref.take(chunk_size);
    let len = handle
        .read_to_end(&mut buf)
        .await
        .context("Cannot read log file chunk")?;
    Ok((buf, len))
}

async fn write_to_file(
    file: &mut File,
    write_position: u64,
    buf: &[u8],
) -> Result<(), anyhow::Error> {
    file.seek(io::SeekFrom::Start(write_position))
        .await
        .context("Failed to seek log file")?;
    file.write_all(buf).await?;
    file.flush().await?;
    Ok(())
}

async fn upload_write(manifold_path: String, buf: &[u8]) -> anyhow::Result<()> {
    // TODO T149151673: support windows uploads
    let upload = manifold::curl_write_command(
        manifold::Bucket::EventLogs.info(),
        &manifold_path,
        MANIFOLD_TTL_S.get_copied()?,
    )?;
    if let Some(mut upload) = upload {
        let mut child = upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .stdin(Stdio::piped())
            .spawn()
            .context("Error spawning command")?;
        let stdin = child.stdin.as_mut().expect("stdin was piped");
        stdin.write_all(buf).await?;
        let exit_code_error = |code: i32, stderr: String| {
            manifold::UploadError::StreamUploadExitCode { code, stderr }
        };
        manifold::wait_for_command(None, child, exit_code_error).await?;
    }
    Ok(())
}

async fn upload_append(
    manifold_path: String,
    buf: &[u8],
    offset: u64,
    cert: &OsString,
) -> anyhow::Result<()> {
    // TODO T149151673: support windows uploads
    let upload = manifold::curl_append_command(
        manifold::Bucket::EventLogs.info(),
        &manifold_path,
        offset,
        cert,
    )?;
    if let Some(mut upload) = upload {
        let mut child = upload
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .stdin(Stdio::piped())
            .spawn()
            .context("Error spawning command")?;
        let stdin = child.stdin.as_mut().expect("stdin was piped");
        stdin.write_all(buf).await?;
        let exit_code_error = |code: i32, stderr: String| {
            manifold::UploadError::StreamUploadExitCode { code, stderr }
        };
        manifold::wait_for_command(None, child, exit_code_error).await?;
    }
    Ok(())
}
