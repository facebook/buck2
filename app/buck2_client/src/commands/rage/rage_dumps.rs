/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use anyhow::Context;
use buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use buck2_cli_proto::UnstableDiceDumpRequest;
use buck2_client_ctx::daemon::client::connect::BootstrapBuckdClient;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::manifold;
use buck2_core::fs::fs_util::create_dir_all;
use buck2_core::fs::fs_util::remove_all;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_util::process::background_command;

pub async fn upload_dice_dump(
    buckd: BootstrapBuckdClient,
    buck_out_dice: AbsNormPathBuf,
    manifold_id: &String,
) -> anyhow::Result<String> {
    let buckd = buckd.with_subscribers(Default::default());
    let manifold_filename = &format!("{}_dice-dump.tar", manifold_id);
    let this_dump_folder_name = chrono::Utc::now().format("%Y-%m-%dT%H:%M:%S").to_string();
    DiceDump::new(buck_out_dice, &this_dump_folder_name)
        .upload(buckd, manifold_filename)
        .await?;

    Ok(format!("buck2_rage_dumps/flat/{}", manifold_filename))
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
        mut buckd: BuckdClientConnector<'_>,
        manifold_filename: &str,
    ) -> anyhow::Result<()> {
        buck2_client_ctx::eprintln!("Generating Buck2 DICE dump...")?;
        create_dir_all(&self.buck_out_dice).with_context(|| {
            format!(
                "Failed to create directory `{}`, no DICE dump will be created",
                self.buck_out_dice.display()
            )
        })?;

        buckd
            .with_flushing()
            .unstable_dice_dump(UnstableDiceDumpRequest {
                destination_path: self.dump_folder.to_str().unwrap().to_owned(),
                format: DiceDumpFormat::Tsv.into(),
            })
            .await
            .with_context(|| {
                format!(
                    "DICE dump at `{}` failed to complete",
                    self.dump_folder.display()
                )
            })?;

        // create DICE dump name using the old command being rage on and the trace id of this rage command.

        buck2_client_ctx::eprintln!(
            "Compressed DICE dump being uploaded to manifold as {}...",
            &manifold_filename
        )?;
        upload_to_manifold(&self.dump_folder, manifold_filename)
            .await
            .with_context(|| "Failed during manifold upload!")?;

        Ok(())
    }
}

async fn upload_to_manifold(dump_folder: &Path, manifold_filename: &str) -> anyhow::Result<()> {
    if !cfg!(target_os = "windows") {
        buck2_core::facebook_only();

        let tar = background_command("tar")
            .arg("-c")
            .arg(dump_folder)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .spawn()?;

        manifold::Upload::new(manifold::Bucket::RAGE_DUMPS, manifold_filename)
            .with_default_ttl()
            .from_stdio(tar.stdout.unwrap().into())?
            .spawn()
            .await?;
    }
    Ok(())
}

impl Drop for DiceDump {
    fn drop(&mut self) {
        if let Err(e) = remove_all(&self.dump_folder).with_context(|| {
            format!(
                "Failed to remove Buck2 DICE dump folder at `{}`. Please remove this manually as it could be quite large.",
                self.dump_folder.display()
            )
        }) { tracing::warn!("{:#}", e); };
    }
}
