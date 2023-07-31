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
use buck2_client_ctx::manifold::Bucket;
use buck2_client_ctx::manifold::ManifoldClient;
use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use tokio::io::BufReader;

#[derive(Debug, clap::Parser)]
#[clap(about = "upload RE logs")]
pub struct UploadReLogsCommand {
    #[clap(long)]
    session_id: String,

    #[clap(long)]
    allow_vpnless: bool,
}

impl UploadReLogsCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        buck2_core::facebook_only();

        // TODO: This should receive the path from the caller.
        ctx.with_runtime(async move |ctx| {
            let manifold = ManifoldClient::new(self.allow_vpnless)?;
            let re_logs_dir = ctx.paths()?.re_logs_dir();
            upload_re_logs(
                &manifold,
                Bucket::RE_LOGS,
                &re_logs_dir,
                &self.session_id,
                &format!("flat/{}.log.zst", &self.session_id),
            )
            .await?;
            ExitResult::success()
        })
    }
}

pub(crate) async fn upload_re_logs(
    manifold: &ManifoldClient,
    bucket: Bucket,
    re_logs_dir: &AbsNormPath,
    session_id: &str,
    bucket_path: &str,
) -> anyhow::Result<()> {
    let logs_path = re_logs_dir
        .join(ForwardRelativePath::new(session_id)?)
        .join(ForwardRelativePath::new("REClientFolly.log")?);
    let file = async_fs_util::open(&logs_path).await?;
    let mut encoder =
        ZstdEncoder::with_quality(BufReader::new(file), async_compression::Level::Default);

    manifold
        .read_and_upload(bucket, bucket_path, Default::default(), &mut encoder)
        .await?;

    Ok(())
}
