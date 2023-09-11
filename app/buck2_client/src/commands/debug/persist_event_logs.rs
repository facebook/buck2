/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_client_ctx::chunk_reader::ChunkReader;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::manifold;
use buck2_client_ctx::manifold::ManifoldChunkedUploader;
use buck2_client_ctx::manifold::ManifoldClient;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::soft_error;
use thiserror::Error;
use tokio::fs::File;
use tokio::fs::OpenOptions;
use tokio::io;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncSeekExt;
use tokio::io::AsyncWriteExt;
use tokio::sync::Mutex;

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
    #[clap(long, help = "Allow vpnless")]
    allow_vpnless: bool,
}

impl PersistEventLogsCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        buck2_core::facebook_only();
        if let Err(e) = ctx.with_runtime(async move |mut ctx| {
            let mut stdin = io::BufReader::new(ctx.stdin());
            self.write_and_upload(&mut stdin).await
        }) {
            soft_error!(categorize_error(&e), e)?;
        };
        ExitResult::success()
    }

    async fn write_and_upload(self, stdin: impl io::AsyncBufRead + Unpin) -> anyhow::Result<()> {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let file = Mutex::new(create_log_file(self.local_path).await?);

        let write = write_task(&file, tx, stdin);
        let upload = upload_task(
            &file,
            rx,
            self.manifold_name,
            self.no_upload,
            self.allow_vpnless,
        );

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
    allow_vpnless: bool,
) -> anyhow::Result<()> {
    if no_upload {
        return Ok(());
    }

    let manifold_client = ManifoldClient::new(allow_vpnless)?;
    let manifold_path = format!("flat/{}", manifold_name);
    let mut uploader = Uploader::new(file_mutex, &manifold_path, &manifold_client)?;

    while let Some(n) = rx.recv().await {
        uploader.bump_total_bytes(n);
        while uploader.can_fill_chunk()? {
            uploader.upload_chunk().await?;
        }
    }

    // When tx gets dropped, rx will return None
    while uploader.can_fill_chunk()? {
        uploader.upload_chunk().await?;
    }

    // Last chunk to upload is smaller than &reader
    uploader.upload_chunk().await?;

    Ok(())
}

/// Provides methods to:
/// - decide when to upload a chunk of the log file
/// - do the actual upload, which is mostly managed by `ManifoldChunkedUploader`
struct Uploader<'a> {
    file_mutex: &'a Mutex<File>,
    manifold: ManifoldChunkedUploader<'a>,
    reader: ChunkReader,
    total_bytes: u64,
}

impl<'a> Uploader<'a> {
    fn new(
        file_mutex: &'a Mutex<File>,
        manifold_path: &'a str,
        manifold_client: &'a ManifoldClient,
    ) -> anyhow::Result<Self> {
        let ttl = MANIFOLD_TTL_S.get_copied()?.map(manifold::Ttl::from_secs);

        let manifold = manifold_client.start_chunked_upload(
            manifold::Bucket::EVENT_LOGS,
            manifold_path,
            ttl.unwrap_or_default(),
        );

        Ok(Self {
            file_mutex,
            manifold,
            reader: ChunkReader::new()?,
            total_bytes: 0,
        })
    }

    /// Uploads at most 'chunk size' bytes to Manifold
    /// Also updates total_bytes counter
    async fn upload_chunk(&mut self) -> anyhow::Result<()> {
        let mut file = self.file_mutex.lock().await;
        file.seek(io::SeekFrom::Start(self.manifold.position()))
            .await
            .context("Failed to seek log file")?;
        let buf = self.reader.read(&mut *file).await?;
        drop(file);

        self.manifold.write(buf.into()).await?;
        Ok(())
    }

    fn bump_total_bytes(&mut self, n: u64) {
        self.total_bytes += n
    }

    fn can_fill_chunk(&mut self) -> anyhow::Result<bool> {
        Ok(self
            .total_bytes
            .checked_sub(self.manifold.position())
            .ok_or(PersistLogError::ReadBytesOverflow)?
            > self.reader.chunk_size())
    }
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

fn categorize_error(err: &anyhow::Error) -> &'static str {
    // This is for internal error tracking in `logview buck2`
    // Each category should point to 1 root cause
    // In case any of this is to be changed, just give a heads up
    // to anybody who may be actively tracking these errors
    let err_msg = format!("{:#}", err);
    if err_msg.contains("CertificateRequired") {
        "persist_log_certificate_required"
    } else if err_msg.contains("No space left on device") {
        "persist_log_no_space_on_device"
    } else if err_msg.contains("Timed out while making request") {
        "persist_log_http_write_timeout"
    } else if err_msg.contains("Input/output error") {
        "persist_log_input_output_error"
    } else if err_msg.contains("failed to lookup address information") {
        "persist_log_dns_failed_lookup"
    } else if err_msg.contains("BadCertificate") {
        "persist_log_bad_certificate"
    } else if err_msg.contains("connection reset") {
        "persist_log_connection_reset"
    } else if err_msg.contains("Broken pipe") {
        "persist_log_broken_pipe"
    } else if err_msg.contains("Error loading system root certificates") {
        "persist_log_error_loading_system_root_certs"
    } else {
        "persist_log_other"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_categorize_error() {
        let err = anyhow::anyhow!("CertificateRequired");
        assert_eq!(categorize_error(&err), "persist_log_certificate_required");

        let err = anyhow::anyhow!("Some other error");
        assert_eq!(categorize_error(&err), "persist_log_other");
    }
}
