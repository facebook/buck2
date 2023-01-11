/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::daemon::client::connect::BuckdConnectOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::manifold;
use buck2_core::fs::fs_util::create_dir_all;
use buck2_core::fs::fs_util::remove_dir_all;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::process::background_command;
use buck2_events::trace::TraceId;
use cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use cli_proto::UnstableDiceDumpRequest;

#[derive(Debug, thiserror::Error)]
enum DumpError {
    #[error("Failed to upload DICE dump folder `{0}` to manifold with exit code {1}")]
    ManifoldUploadWithExitCodeError(String, i32),
    #[error("Failed to upload DICE dump folder `{0}` to manifold due to signal interrupt")]
    ManifoldUploadSignalInterruptError(String),
    #[error("Failed to find suitable Manifold upload command")]
    ManifoldUploadCommandNotFound,
}

pub async fn upload_dice_dump(
    ctx: &ClientCommandContext,
    new_trace_id: &TraceId,
    old_trace_id: &TraceId,
) -> anyhow::Result<String> {
    let buckd = ctx
        .connect_buckd(BuckdConnectOptions::existing_only_no_console())
        .await?;
    let this_dump_folder_name = chrono::Utc::now().format("%Y-%m-%dT%H:%M:%S").to_string();
    let buck_out_dice = ctx.paths.dice_dump_dir();

    let manifold_filename = format!("{}_{}_dice-dump.gz", old_trace_id, new_trace_id);

    DiceDump::new(buck_out_dice, &this_dump_folder_name)
        .upload(buckd, &manifold_filename)
        .await?;

    Ok(format!("buck2_dice_dump/flat/{}", manifold_filename))
}

struct DiceDump {
    buck_out_dice: AbsNormPathBuf,
    dump_folder: PathBuf,
}

impl DiceDump {
    fn new(buck_out_dice: AbsNormPathBuf, dump_folder_name: &str) -> Self {
        let dump_folder = buck_out_dice.as_path().join(Path::new(dump_folder_name));

        Self {
            buck_out_dice,
            dump_folder,
        }
    }

    async fn upload(
        &self,
        mut buckd: BuckdClientConnector,
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

        remove_dir_all(&self.dump_folder).with_context(|| {
        format!(
            "Failed to remove Buck2 DICE dump folder at `{}`. Please remove this manually as it could be quite large.",
            self.dump_folder.display()
        )
        })?;
        Ok(())
    }
}

async fn upload_to_manifold(dump_folder: &Path, manifold_filename: &str) -> anyhow::Result<()> {
    if !cfg!(target_os = "windows") {
        buck2_core::facebook_only();

        let tar_gzip = background_command("tar")
            .arg("-c")
            .arg(dump_folder)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .spawn()?;

        let mut upload =
            manifold::upload_command("buck2_dice_dump", manifold_filename, "buck2_dice_dump-key")?
                .context(DumpError::ManifoldUploadCommandNotFound)?;
        upload.stdin(tar_gzip.stdout.unwrap());
        let exit_code_result = upload.spawn()?.wait().await?.code();

        match exit_code_result {
            Some(code) => match code {
                0 => {}
                e => {
                    return Err(DumpError::ManifoldUploadWithExitCodeError(
                        dump_folder.display().to_string(),
                        e,
                    )
                    .into());
                }
            },
            None => {
                return Err(DumpError::ManifoldUploadSignalInterruptError(
                    dump_folder.display().to_string(),
                )
                .into());
            }
        }
    }
    Ok(())
}
