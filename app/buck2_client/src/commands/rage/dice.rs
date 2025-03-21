/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use buck2_cli_proto::UnstableDiceDumpRequest;
use buck2_client_ctx::daemon::client::connect::BootstrapBuckdClient;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_common::manifold::Bucket;
use buck2_common::manifold::ManifoldClient;
use buck2_core::fs::fs_util::create_dir_all;
use buck2_core::fs::fs_util::remove_all;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_error::BuckErrorContext;
use buck2_util::process::async_background_command;

use crate::commands::rage::manifold::manifold_leads;

pub async fn upload_dice_dump(
    buckd: BootstrapBuckdClient,
    buck_out_dice: AbsNormPathBuf,
    manifold: &ManifoldClient,
    manifold_id: &String,
) -> buck2_error::Result<String> {
    let buckd = buckd.to_connector();
    let mut events_ctx = EventsCtx::new(Default::default());
    let manifold_bucket = Bucket::RAGE_DUMPS;
    let manifold_filename = format!("flat/{}_dice-dump.tar", manifold_id);
    let this_dump_folder_name = chrono::Utc::now().format("%Y-%m-%dT%H:%M:%S").to_string();
    DiceDump::new(buck_out_dice, &this_dump_folder_name)
        .upload(
            buckd,
            &mut events_ctx,
            manifold,
            manifold_bucket,
            &manifold_filename,
        )
        .await?;

    Ok(manifold_leads(&manifold_bucket, manifold_filename))
}

struct DiceDump {
    buck_out_dice: AbsNormPathBuf,
    dump_folder: AbsPathBuf,
}

impl DiceDump {
    fn new(buck_out_dice: AbsNormPathBuf, dump_folder_name: &str) -> Self {
        let dump_folder = buck_out_dice.as_abs_path().join(dump_folder_name);

        Self {
            buck_out_dice,
            dump_folder,
        }
    }

    async fn upload(
        &self,
        mut buckd: BuckdClientConnector,
        events_ctx: &mut EventsCtx,
        manifold: &ManifoldClient,
        manifold_bucket: Bucket,
        manifold_filename: &str,
    ) -> buck2_error::Result<()> {
        create_dir_all(&self.buck_out_dice).with_buck_error_context(|| {
            format!(
                "Failed to create directory `{}`, no DICE dump will be created",
                self.buck_out_dice.display()
            )
        })?;

        buckd
            .with_flushing()
            .unstable_dice_dump(
                UnstableDiceDumpRequest {
                    destination_path: self.dump_folder.to_str().unwrap().to_owned(),
                    format: DiceDumpFormat::Tsv.into(),
                },
                events_ctx,
            )
            .await
            .with_buck_error_context(|| {
                format!(
                    "DICE dump at `{}` failed to complete",
                    self.dump_folder.display()
                )
            })?;

        // create DICE dump name using the old command being rage on and the trace id of this rage command.
        upload_to_manifold(
            &self.dump_folder,
            manifold,
            manifold_bucket,
            manifold_filename,
        )
        .await
        .with_buck_error_context(|| "Failed during manifold upload!")?;

        Ok(())
    }
}

async fn upload_to_manifold(
    dump_folder: &Path,
    manifold: &ManifoldClient,
    manifold_bucket: Bucket,
    manifold_filename: &str,
) -> buck2_error::Result<()> {
    if !cfg!(target_os = "windows") {
        buck2_core::facebook_only();

        let tar = async_background_command("tar")
            .arg("-c")
            .arg(dump_folder)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .spawn()?;

        manifold
            .read_and_upload(
                manifold_bucket,
                manifold_filename,
                Default::default(),
                &mut tar.stdout.unwrap(),
            )
            .await?;
    }
    Ok(())
}

impl Drop for DiceDump {
    fn drop(&mut self) {
        if let Err(e) = remove_all(&self.dump_folder).with_buck_error_context(|| {
            format!(
                "Failed to remove Buck2 DICE dump folder at `{}`. Please remove this manually as it could be quite large.",
                self.dump_folder.display()
            )
        }) { tracing::warn!("{:#}", e); };
    }
}
