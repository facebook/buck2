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
use clap::ArgMatches;
use tokio::fs::File;
use tokio::fs::OpenOptions;
use tokio::io;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncSeekExt;
use tokio::io::AsyncWriteExt;
use tokio::runtime::Builder;

const UPLOAD_CHUNK_SIZE: u64 = 8 * 1024 * 1024; // 8 MB
type MyJoinHandle = Option<tokio::task::JoinHandle<Result<(), anyhow::Error>>>;

#[derive(Debug, clap::Parser)]
#[clap(about = "Command to stream event logs to Manifold")]
pub struct PersistEventLogsCommand {
    manifold_path: String,
    local_path: String,
}

impl PersistEventLogsCommand {
    pub fn exec(self, _matches: &ArgMatches, mut ctx: ClientCommandContext) -> ExitResult {
        buck2_core::facebook_only();
        let runtime = Builder::new_current_thread()
            .enable_all()
            .build()
            .context("Error building runtime")?;
        let mut stdin = io::BufReader::new(ctx.stdin());
        runtime
            .block_on(self.write(&mut stdin))
            .context("Error writing")?;
        ExitResult::success()
    }

    async fn write(self, stdin: impl io::AsyncBufRead + Unpin) -> anyhow::Result<()> {
        let mut file = self.create_log_file().await?;
        let mut read_position: u64 = 0;
        let mut upload_handle: MyJoinHandle = None;
        let mut first_upload = true;
        poll_and_upload_loop(
            stdin,
            &mut file,
            &mut upload_handle,
            &mut read_position,
            &mut first_upload,
            &self.manifold_path,
        )
        .await?;
        upload_remaining(
            &mut file,
            upload_handle,
            read_position,
            first_upload,
            &self.manifold_path,
        )
        .await?;
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
    upload_handle: &mut MyJoinHandle,
    read_position: &mut u64,
    first_upload: &mut bool,
    manifold_path: &str,
) -> Result<(), anyhow::Error> {
    let mut write_position: u64 = 0;
    loop {
        let mut buf = vec![0; 64 * 1024]; // maximum pipe size in linux
        let bytes_read = stdin.read(&mut buf).await?;
        if bytes_read == 0 {
            break; // closed stdin
        }
        write_to_file(file, write_position, &buf[..bytes_read]).await?;
        write_position += bytes_read as u64;
        let cert = find_certs::find_tls_cert()?;
        if should_upload(*read_position, write_position, &*upload_handle) {
            file.seek(io::SeekFrom::Start(*read_position))
                .await
                .context("Failed to seek log file")?;
            let (buf, len) = read_chunk(file).await?;
            // Upload concurrently
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
                    if let Err(e) =
                        upload_append(manifold_path, &buf, read_position_clone, &cert).await
                    {
                        let _res = buck2_core::soft_error!("manifold_append_error", e);
                    }
                    Ok(())
                }));
            }
            *read_position += len as u64;
        }
    }
    Ok(())
}

async fn upload_remaining(
    file: &mut File,
    last_upload_handle: MyJoinHandle,
    mut read_position: u64,
    mut first_upload: bool,
    manifold_path: &str,
) -> Result<(), anyhow::Error> {
    if let Some(handle) = last_upload_handle {
        handle.await??;
    };
    file.seek(io::SeekFrom::Start(read_position))
        .await
        .context("Failed to seek log file")?;
    let cert = find_certs::find_tls_cert()?;
    loop {
        // Upload in chunks size UPLOAD_CHUNK_SIZE
        let (buf, len) = read_chunk(file).await?;
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

async fn read_chunk(read_ref: &mut File) -> Result<(Vec<u8>, usize), anyhow::Error> {
    let mut buf = vec![];
    let mut handle = read_ref.take(UPLOAD_CHUNK_SIZE);
    let len = handle
        .read_to_end(&mut buf)
        .await
        .context("Cannot read log file chunk")?;
    Ok((buf, len))
}

fn should_upload(read_position: u64, write_position: u64, upload_handle: &MyJoinHandle) -> bool {
    write_position - read_position > UPLOAD_CHUNK_SIZE
        && upload_handle.as_ref().map_or(true, |h| h.is_finished())
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
    let upload =
        manifold::curl_write_command(manifold::Bucket::EventLogs.info(), &manifold_path, None)?;
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
