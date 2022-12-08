/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use chrono::DateTime;
use chrono::Utc;
use derivative::Derivative;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::runtime::Handle;
use tokio::sync::oneshot::Sender;

use crate::materializers::deferred::clean_output_paths;
use crate::materializers::deferred::extension::ExtensionCommand;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::CleaningFuture;
use crate::materializers::deferred::DeferredMaterializerCommandProcessor;
use crate::materializers::deferred::DeferredMaterializerIoHandler;
use crate::materializers::sqlite::MaterializerStateSqliteDb;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct CleanStaleArtifacts {
    pub keep_since_time: DateTime<Utc>,
    pub dry_run: bool,
    #[derivative(Debug = "ignore")]
    pub sender: Sender<BoxFuture<'static, anyhow::Result<String>>>,
}

impl ExtensionCommand for CleanStaleArtifacts {
    fn execute(
        self: Box<Self>,
        tree: &mut ArtifactTree,
        processor: &mut DeferredMaterializerCommandProcessor,
    ) {
        let res = gather_clean_futures_for_stale_artifacts(
            tree,
            self.keep_since_time,
            self.dry_run,
            &mut processor.sqlite_db,
            &processor.io,
            &processor.rt,
        );
        let fut = async move {
            let (cleaning_futs, output) = res?;
            for t in cleaning_futs {
                t.await?;
            }
            tracing::trace!("finished cleaning stale artifacts");
            Ok(output)
        }
        .boxed();
        let _ignored = self.sender.send(fut);
    }
}

fn gather_clean_futures_for_stale_artifacts(
    tree: &mut ArtifactTree,
    keep_since_time: DateTime<Utc>,
    dry_run: bool,
    sqlite_db: &mut Option<MaterializerStateSqliteDb>,
    io: &Arc<DeferredMaterializerIoHandler>,
    rt: &Handle,
) -> anyhow::Result<(Vec<CleaningFuture>, String)> {
    let mut output = String::new();
    let mut stale_count = 0;
    let mut retained_count = 0;
    let mut paths_to_clean: Vec<ProjectRelativePathBuf> = Vec::new();

    for (k, v) in tree.iter() {
        if let ArtifactMaterializationStage::Materialized {
            last_access_time,
            active,
            ..
        } = &v.stage
        {
            let f_path = k
                .iter()
                .map(|f| f.as_ref())
                .collect::<Option<ForwardRelativePathBuf>>()
                .context("Invalid path key.")?;

            let path = ProjectRelativePathBuf::from(f_path);
            if *last_access_time < keep_since_time && !active {
                stale_count += 1;
                tracing::trace!(path = %path, "stale artifact");
                paths_to_clean.push(path);
            } else {
                tracing::trace!(path = %path, "retaining artifact");
                retained_count += 1;
            }
        }
    }
    // TODO(ctolliday) clean untracked file that are not inside of retained dirs or referenced by retained symlinks

    writeln!(
        output,
        "Found {} stale artifacts and {} recent artifacts",
        stale_count, retained_count,
    )?;
    let mut cleaning_futs = Vec::new();
    if !dry_run {
        writeln!(output, "Cleaning {} artifacts.", paths_to_clean.len())?;

        for path in paths_to_clean {
            let existing_futs =
                tree.invalidate_paths_and_collect_futures(vec![path.clone()], sqlite_db.as_mut());

            cleaning_futs.push(clean_output_paths(&io.io_executor, path, existing_futs, rt));
        }
    }

    Ok((cleaning_futs, output))
}
