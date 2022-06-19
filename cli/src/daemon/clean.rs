/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::{Arc, Mutex};

use anyhow::Context as _;
use buck2_build_api::context::HasBuildContextData;
use buck2_core::fs::{anyhow as fs, paths::AbsPathBuf};
use cli_proto::{CleanRequest, CleanResponse};
use gazebo::prelude::*;
use threadpool::ThreadPool;
use walkdir::WalkDir;

use crate::daemon::server::ServerCommandContext;

pub(crate) async fn clean(
    server_ctx: ServerCommandContext,
    request: CleanRequest,
) -> anyhow::Result<CleanResponse> {
    let dice_ctx = server_ctx.dice_ctx().await?;
    let out_path = dice_ctx.get_buck_out_path().await;
    let filesystem = server_ctx.file_system();
    let buck_out_path = (*out_path).resolve(&filesystem.root);

    tokio::task::spawn_blocking(move || {
        if !buck_out_path.exists() {
            return Ok(CleanResponse {
                clean_paths: vec![],
            });
        }

        let clean_paths =
            collect_clean_paths(&buck_out_path)?.map(|path| path.display().to_string());
        if !request.dry_run {
            clean_buck_out(&buck_out_path)?;
        }
        Ok(CleanResponse { clean_paths })
    })
    .await
    .context("Failed to spawn clean")?
}

fn collect_clean_paths(buck_out_path: &AbsPathBuf) -> anyhow::Result<Vec<AbsPathBuf>> {
    let mut clean_paths = vec![];
    let dir = fs::read_dir(buck_out_path)?;
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
            thread_pool.execute(move || match fs::remove_file(dir_entry.path()) {
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
        fs::remove_dir(path)?;
    }

    Ok(())
}
