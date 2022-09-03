/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Mutex;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::context::HasBuildContextData;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use cli_proto::CleanRequest;
use cli_proto::CleanResponse;
use dice::DiceTransaction;
use gazebo::prelude::*;
use threadpool::ThreadPool;
use walkdir::WalkDir;

pub async fn clean_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: cli_proto::CleanRequest,
) -> anyhow::Result<cli_proto::CleanResponse> {
    run_server_command(CleanServerCommand { req }, ctx).await
}

struct CleanServerCommand {
    req: cli_proto::CleanRequest,
}

#[async_trait]
impl ServerCommandTemplate for CleanServerCommand {
    type StartEvent = buck2_data::CleanCommandStart;
    type EndEvent = buck2_data::CleanCommandEnd;
    type Response = cli_proto::CleanResponse;

    async fn command(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        clean(server_ctx, ctx, &self.req).await
    }
}

async fn clean(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    dice_ctx: DiceTransaction,
    request: &CleanRequest,
) -> anyhow::Result<CleanResponse> {
    let out_path = dice_ctx.get_buck_out_path().await?;
    let filesystem = server_ctx.project_root();
    let buck_out_path = out_path
        .as_forward_relative_path()
        .resolve(filesystem.root());

    // Try to clean EdenFS based buck-out first. For EdenFS based buck-out, "eden rm"
    // is effecient. Notice eden rm will remove the buck-out root direcotry,
    // but for the native fs, the buck-out root directory is kept.
    if let Some(clean_paths) = try_clean_eden_buck_out(&buck_out_path, request.dry_run).await? {
        return Ok(CleanResponse { clean_paths });
    }

    let dry_run = request.dry_run;
    tokio::task::spawn_blocking(move || {
        if !buck_out_path.exists() {
            return Ok(CleanResponse {
                clean_paths: vec![],
            });
        }

        let clean_paths =
            collect_clean_paths(&buck_out_path)?.map(|path| path.display().to_string());
        if !dry_run {
            clean_buck_out(&buck_out_path)?;
        }
        Ok(CleanResponse { clean_paths })
    })
    .await
    .context("Failed to spawn clean")?
}

fn collect_clean_paths(buck_out_path: &AbsPathBuf) -> anyhow::Result<Vec<AbsPathBuf>> {
    let mut clean_paths = vec![];
    let dir = fs_util::read_dir(buck_out_path)?;
    for entry in dir {
        let entry = entry?;
        let path = entry.path();
        let abs_path = AbsPathBuf::try_from(path)?;
        clean_paths.push(abs_path);
    }

    Ok(clean_paths)
}

fn clean_buck_out(path: &AbsPathBuf) -> anyhow::Result<()> {
    let walk = WalkDir::new(path);
    let thread_pool = ThreadPool::new(num_cpus::get());
    let error = Arc::new(Mutex::new(None));
    // collect dir paths to delete them after deleting files in them
    // we need reverse order to make sure the dir is already empty when
    // we delete it, otherwise remove would fail with DirNotEmpty exception
    let mut reverse_dir_paths = Vec::new();
    for dir_entry in walk.into_iter().flatten() {
        if dir_entry.file_type().is_dir() {
            reverse_dir_paths.push(dir_entry.into_path());
        } else {
            let error = error.dupe();
            thread_pool.execute(move || match fs_util::remove_file(dir_entry.path()) {
                Ok(_) => {}
                Err(e) => {
                    let mut error = error.lock().unwrap();
                    if error.is_none() {
                        *error = Some(e);
                    }
                }
            })
        }
    }

    thread_pool.join();
    if let Some(e) = error.lock().unwrap().take() {
        return Err(e);
    }

    // first entry is buck-out root dir and we don't want to remove it
    for path in reverse_dir_paths.iter().skip(1).rev() {
        fs_util::remove_dir(path)?;
    }

    Ok(())
}

#[cfg(all(unix, feature = "eden_materializer"))]
async fn try_clean_eden_buck_out(
    buck_out: &AbsPathBuf,
    dryrun: bool,
) -> anyhow::Result<Option<Vec<String>>> {
    use std::process::Stdio;

    use buck2_core::process::async_background_command;
    use buck2_execute::materialize::eden_api::is_recas_eden_mount;

    if !is_recas_eden_mount(buck_out)? {
        return Ok(None);
    }

    // Run eden rm <buck-out> to rm a mount
    let mut eden_rm_cmd = async_background_command("eden");
    eden_rm_cmd
        .arg("rm")
        .arg("-y") // No promot
        .arg(buck_out.as_os_str())
        .current_dir("/")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    eprintln!(
        "The following command will be executed: `eden rm -y {}`",
        buck_out
    );

    if !dryrun {
        eden_rm_cmd
            .spawn()
            .context("Failed to start to remove EdenFS buck-out mount.")?
            .wait()
            .await
            .context("Failed to remove EdenFS buck-out mount.")?;

        // eden rm might not delete the buck-out completed.
        if buck_out.exists() {
            fs_util::remove_dir(buck_out)?;
        }
    }

    Ok(Some(vec![buck_out.display().to_string()]))
}

#[cfg(any(not(feature = "eden_materializer"), not(unix)))]
async fn try_clean_eden_buck_out(
    _buck_out: &AbsPathBuf,
    _dryrun: bool,
) -> anyhow::Result<Option<Vec<String>>> {
    #[cfg(all(fbcode_build, unix))]
    compile_error!("eden_materializer must be enabled when compiling in fbcode");
    Ok(None)
}
