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
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use clap::ArgMatches;
use thiserror::Error;
use tokio::fs::File;
use tokio::fs::OpenOptions;
use tokio::io;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncSeekExt;
use tokio::io::AsyncWriteExt;
use tokio::sync::Mutex;

static UPLOAD_CHUNK_SIZE: EnvHelper<u64> = EnvHelper::new("BUCK2_TEST_MANIFOLD_CHUNK_BYTES");
static MANIFOLD_TTL_S: EnvHelper<u64> = EnvHelper::new("BUCK2_TEST_MANIFOLD_TTL_S");

#[derive(Debug, Error)]
pub(crate) enum PersistLogError {
    #[error("Read more bytes than are available")]
    ReadBytesOverflow,
}

/// Read binary event log from stdin and simultaneously write it to disk and optionally upload to Manifold.
///
/// This command is launched by the buck client to continue log streaming
/// after client command finishes. It is not intended to be used directly.
#[derive(Debug, clap::Parser)]
pub struct PersistEventLogsCommand {
    #[clap(long, help = "Name this log will take in Manifold")]
    manifold_name: String,
    #[clap(long, help = "Where to write this log to on disk")]
    local_path: AbsPathBuf,
    #[clap(long, help = "If present, only write to disk and don't upload")]
    no_upload: bool,
}

impl PersistEventLogsCommand {
    pub fn exec(self, _matches: &ArgMatches, mut ctx: ClientCommandContext<'_>) -> ExitResult {
        buck2_core::facebook_only();
        let runtime = client_tokio_runtime()?;
        let mut stdin = io::BufReader::new(ctx.stdin());
        runtime
            .block_on(self.write_and_upload(&mut stdin))
            .context("Error writing or uploading event log")?;
        ExitResult::success()
    }

    async fn write_and_upload(self, stdin: impl io::AsyncBufRead + Unpin) -> anyhow::Result<()> {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let file = Mutex::new(create_log_file(self.local_path).await?);

        let write = write_task(&file, tx, stdin);
        let upload = upload_task(&file, rx, self.manifold_name, self.no_upload);

        // Wait for both tasks to finish. If the upload fails we want to keep writing to disk
        let (write_result, upload_result) = tokio::join!(write, upload);
        write_result?;
        upload_result?;
        Ok(())
    }
}

async fn write_task(
    file_mutex: &Mutex<File>,
    tx: tokio::sync::mpsc::UnboundedSender<u64>,
    mut stdin: impl io::AsyncBufRead + Unpin,
) -> anyhow::Result<()> {
    let mut write_position = 0;
    loop {
        let mut buf = vec![0; 64 * 1024]; // maximum pipe size in linux
        let bytes_read = stdin.read(&mut buf).await?;
        if bytes_read == 0 {
            break; // closed stdin
        }
        let mut file = file_mutex.lock().await;
        write_to_file(&mut file, write_position, &buf[..bytes_read]).await?;
        drop(file);
        write_position += bytes_read as u64;
        let _ignored = tx.send(bytes_read as u64); // If this errors, that means the upload task died.
    }

    Ok(())
}

async fn create_log_file(local_path: AbsPathBuf) -> Result<tokio::fs::File, anyhow::Error> {
    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .read(true)
        .open(&local_path)
        .await
        .with_context(|| {
            format!(
                "Failed to open event log for writing at `{}`",
                local_path.display()
            )
        })?;
    Ok(file)
}

async fn upload_task(
    file_mutex: &Mutex<File>,
    mut rx: tokio::sync::mpsc::UnboundedReceiver<u64>,
    manifold_name: String,
    no_upload: bool,
) -> anyhow::Result<()> {
    if no_upload {
        return Ok(());
    }
    let manifold_path = format!("flat/{}", manifold_name);
    let upload_chunk_size = UPLOAD_CHUNK_SIZE.get_copied()?.unwrap_or(8 * 1024 * 1024);
    let cert = find_certs::find_meta_internal_tls_cert()?;
    let mut read_position = 0;
    let mut total_bytes = 0_u64;

    while let Some(n) = rx.recv().await {
        total_bytes += n;
        while should_upload_chunk(total_bytes, read_position, upload_chunk_size).await? {
            do_the_upload_and_increment_read_position(
                file_mutex,
                &mut read_position,
                &manifold_path,
                &cert,
                upload_chunk_size,
            )
            .await?;
        }
    }

    // When tx gets dropped, rx will return None
    while should_upload_chunk(total_bytes, read_position, upload_chunk_size).await? {
        do_the_upload_and_increment_read_position(
            file_mutex,
            &mut read_position,
            &manifold_path,
            &cert,
            upload_chunk_size,
        )
        .await?;
    }

    // Last chunk to upload is smaller than UPLOAD_CHUNK_SIZE
    do_the_upload_and_increment_read_position(
        file_mutex,
        &mut read_position,
        &manifold_path,
        &cert,
        upload_chunk_size,
    )
    .await?;

    Ok(())
}

async fn should_upload_chunk(
    total_bytes: u64,
    read_position: u64,
    upload_chunk_size: u64,
) -> anyhow::Result<bool> {
    Ok(total_bytes
        .checked_sub(read_position)
        .ok_or(PersistLogError::ReadBytesOverflow)?
        > upload_chunk_size)
}

async fn do_the_upload_and_increment_read_position(
    file_mutex: &Mutex<File>,
    read_position: &mut u64,
    manifold_path: &str,
    cert: &OsString,
    upload_chunk_size: u64,
) -> anyhow::Result<u64> {
    let mut file = file_mutex.lock().await;
    // Upload chunk
    file.seek(io::SeekFrom::Start(*read_position))
        .await
        .context("Failed to seek log file")?;
    let (buf, len) = read_chunk(&mut file, upload_chunk_size).await?;
    drop(file);
    if *read_position == 0 {
        // First chunk
        upload_write(manifold_path, &buf, cert).await?;
    } else {
        upload_append(manifold_path, &buf, cert, *read_position).await?;
    }
    *read_position += len;
    Ok(len)
}

async fn read_chunk(read_ref: &mut File, chunk_size: u64) -> Result<(Vec<u8>, u64), anyhow::Error> {
    let mut buf = vec![];
    let mut handle = read_ref.take(chunk_size);
    let len = handle
        .read_to_end(&mut buf)
        .await
        .context("Cannot read log file chunk")?;
    Ok((buf, len as u64))
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

async fn upload_write(manifold_path: &str, buf: &[u8], cert: &OsString) -> anyhow::Result<()> {
    // TODO T149151673: support windows uploads
    let upload = manifold::curl_write_command(
        manifold::Bucket::EventLogs.info(),
        manifold_path,
        MANIFOLD_TTL_S.get_copied()?,
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

async fn upload_append(
    manifold_path: &str,
    buf: &[u8],
    cert: &OsString,
    offset: u64,
) -> anyhow::Result<()> {
    // TODO T149151673: support windows uploads
    let upload = manifold::curl_append_command(
        manifold::Bucket::EventLogs.info(),
        manifold_path,
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
