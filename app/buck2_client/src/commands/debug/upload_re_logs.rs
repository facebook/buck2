/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_compression::tokio::bufread::ZstdEncoder;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::manifold;
use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use tokio::io::AsyncRead;
use tokio::io::BufReader;

#[derive(Debug, clap::Parser)]
#[clap(about = "upload RE logs")]
pub struct UploadReLogsCommand {
    #[clap(long)]
    session_id: String,
}

impl UploadReLogsCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        buck2_core::facebook_only();

        ctx.with_runtime(async move |ctx| {
            let re_logs_location = ctx.paths()?.re_logs_dir();
            let logs_path = re_logs_location
                .join(ForwardRelativePath::new(&self.session_id)?)
                .join(ForwardRelativePath::new("REClientFolly.log")?);
            let file = async_fs_util::open(&logs_path).await?;
            let mut encoder =
                ZstdEncoder::with_quality(BufReader::new(file), async_compression::Level::Default);

            self.upload_file(&mut encoder).await?
        })
    }

    async fn upload_file<'a, R>(&self, reader: &'a mut R) -> anyhow::Result<ExitResult>
    where
        R: AsyncRead + Unpin,
    {
        let bucket_path = &format!("{}.log.zst", self.session_id);

        manifold::Upload::new(manifold::Bucket::ReLogs, bucket_path)
            .with_default_ttl()
            .from_async_read(reader)?
            .spawn()
            .await?;
        Ok(ExitResult::success())
    }
}
