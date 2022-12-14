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
use buck2_common::file_ops::FileType;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use chrono::DateTime;
use chrono::Utc;
use derivative::Derivative;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::runtime::Handle;
use tokio::sync::oneshot::Sender;

use crate::materializers::deferred::clean_output_paths;
use crate::materializers::deferred::extension::ExtensionCommand;
use crate::materializers::deferred::file_tree::DataTree;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::CleaningFuture;
use crate::materializers::deferred::DefaultIoHandler;
use crate::materializers::deferred::DeferredMaterializerCommandProcessor;
use crate::materializers::sqlite::MaterializerStateSqliteDb;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct CleanStaleArtifacts {
    pub keep_since_time: DateTime<Utc>,
    pub dry_run: bool,
    pub tracked_only: bool,
    #[derivative(Debug = "ignore")]
    pub sender: Sender<BoxFuture<'static, anyhow::Result<String>>>,
}

impl ExtensionCommand<DefaultIoHandler> for CleanStaleArtifacts {
    fn execute(
        self: Box<Self>,
        tree: &mut ArtifactTree,
        processor: &mut DeferredMaterializerCommandProcessor<DefaultIoHandler>,
    ) {
        let res = gather_clean_futures_for_stale_artifacts(
            tree,
            self.keep_since_time,
            self.dry_run,
            self.tracked_only,
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
    tracked_only: bool,
    sqlite_db: &mut Option<MaterializerStateSqliteDb>,
    io: &Arc<DefaultIoHandler>,
    rt: &Handle,
) -> anyhow::Result<(Vec<CleaningFuture>, String)> {
    let gen_path = &io
        .buck_out_path
        .join(&ProjectRelativePathBuf::unchecked_new("gen".to_owned()));
    let gen_dir = io.fs.resolve(gen_path);
    if !fs_util::try_exists(&gen_dir)? {
        return Ok((vec![], "Nothing to clean".to_owned()));
    }
    tracing::trace!(gen_dir = %gen_dir, "Scanning");

    let result = if tracked_only {
        find_stale_tracked_only(tree, keep_since_time)?
    } else {
        let subtree = tree
            .get_subtree(&mut io.fs.relativize(&gen_dir)?.iter())
            .context("Found a file where gen dir expected")?;
        find_stale_recursive(
            &io.fs,
            subtree,
            &gen_dir,
            keep_since_time,
            StaleFinderResult::new(),
        )?
    };

    let mut output = String::new();
    writeln!(
        output,
        "Found {} stale artifacts {} recent artifacts and {} untracked artifacts",
        result.stale_count, result.retained_count, result.untracked_count
    )?;
    let mut cleaning_futs = Vec::new();
    if !dry_run {
        writeln!(
            output,
            "Cleaning {} artifacts.",
            result.paths_to_clean.len()
        )?;

        for path in result.paths_to_clean {
            let existing_futs =
                tree.invalidate_paths_and_collect_futures(vec![path.clone()], sqlite_db.as_mut());

            cleaning_futs.push(clean_output_paths(io, path, existing_futs, rt));
        }
    }

    Ok((cleaning_futs, output))
}

struct StaleFinderResult {
    stale_count: u64,
    retained_count: u64,
    untracked_count: u64,
    paths_to_clean: Vec<ProjectRelativePathBuf>,
}

impl StaleFinderResult {
    fn new() -> Self {
        Self {
            stale_count: 0,
            retained_count: 0,
            untracked_count: 0,
            paths_to_clean: Vec::new(),
        }
    }
}

fn find_stale_recursive(
    fs: &ProjectRoot,
    subtree: Option<&ArtifactTree>,
    path: &AbsNormPath,
    keep_since_time: DateTime<Utc>,
    mut result: StaleFinderResult,
) -> anyhow::Result<StaleFinderResult> {
    // Use symlink_metadata to not follow symlinks (stale/untracked symlink target may have been cleaned first)
    let path_type = FileType::from(fs_util::symlink_metadata(&path)?.file_type());
    let rel_path = fs.relativize(&path)?;

    let clean_untracked = |mut result: StaleFinderResult| {
        result.untracked_count += 1;
        result.paths_to_clean.push(rel_path.clone().into_owned());
        tracing::trace!(path = %path, path_type = ?path_type, "marking as untracked");
        Ok(result)
    };

    if let Some(DataTree::Data(metadata)) = subtree {
        if let ArtifactMaterializationStage::Materialized {
            last_access_time,
            active,
            ..
        } = metadata.stage
        {
            if last_access_time < keep_since_time && !active {
                result.stale_count += 1;
                result.paths_to_clean.push(rel_path.clone().into_owned());
                tracing::trace!(path = %path, path_type = ?path_type, "marking as stale");
            } else {
                result.retained_count += 1;
                tracing::trace!(path = %path, path_type = ?path_type, "marking as retained");
            }
            Ok(result)
        } else {
            // Artifact was declared but never materialized, should not be a file here.
            clean_untracked(result)
        }
    } else if path_type.is_dir() {
        let children = subtree.and_then(|t| t.children());
        for file in fs_util::read_dir(path)? {
            let child_path = file?.path();
            // If a file or dir exists here but the name not valid utf-8 it must be untracked.
            // Make subtree None to mark it untracked instead of throwing an error.
            let file_name = child_path
                .file_name()
                .and_then(|f| f.to_str())
                .and_then(|f| FileName::new(f).ok());
            let subtree = children.and_then(|c| file_name.and_then(|f| c.get(f)));
            result = find_stale_recursive(fs, subtree, &child_path, keep_since_time, result)?;
        }
        Ok(result)
    } else {
        clean_untracked(result)
    }
}

fn find_stale_tracked_only(
    tree: &ArtifactTree,
    keep_since_time: DateTime<Utc>,
) -> anyhow::Result<StaleFinderResult> {
    let mut result = StaleFinderResult::new();
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
                tracing::trace!(path = %path, "stale artifact");
                result.stale_count += 1;
                result.paths_to_clean.push(path);
            } else {
                tracing::trace!(path = %path, "retaining artifact");
                result.retained_count += 1;
            }
        }
    }
    Ok(result)
}
