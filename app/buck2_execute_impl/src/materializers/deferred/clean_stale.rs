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
        if !processor.defer_write_actions || processor.sqlite_db.is_none() {
            let fut = async move {
                Ok("Skipping clean, set buck2.sqlite_materializer_state and buck2.defer_write_actions to use clean --stale".to_owned())
            }.boxed();
            let _ignored = self.sender.send(fut);
            return;
        }

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

    let mut stats = StaleFinderStats::new();
    let result = if tracked_only {
        find_stale_tracked_only(tree, keep_since_time, &mut stats)?
    } else {
        let subtree = tree
            .get_subtree(&mut io.fs.relativize(&gen_dir)?.iter())
            .context("Found a file where gen dir expected")?;
        find_stale_recursive(&io.fs, subtree, &gen_dir, keep_since_time, &mut stats)?
    };

    let mut output = String::new();
    writeln!(
        output,
        "Found {} stale artifacts ({})",
        stats.stale_count,
        bytesize::to_string(stats.stale_bytes, true),
    )?;
    writeln!(
        output,
        "Found {} recent artifacts ({})",
        stats.retained_count,
        bytesize::to_string(stats.retained_bytes, true),
    )?;
    writeln!(
        output,
        "Found {} untracked artifacts ({})",
        stats.untracked_count,
        bytesize::to_string(stats.untracked_bytes, true),
    )?;
    let mut cleaning_futs = Vec::new();
    if !dry_run {
        let paths_to_clean = result.paths();
        writeln!(
            output,
            "Cleaned {} paths ({} artifacts)",
            paths_to_clean.len(),
            stats.stale_count + stats.untracked_count,
        )?;
        let clean_size = stats.untracked_bytes + stats.stale_bytes;
        writeln!(
            output,
            "{} bytes cleaned ({})",
            clean_size,
            bytesize::to_string(clean_size, true),
        )?;

        for path in paths_to_clean {
            let existing_futs =
                tree.invalidate_paths_and_collect_futures(vec![path.clone()], sqlite_db.as_mut());

            cleaning_futs.push(clean_output_paths(io, path, existing_futs, rt));
        }
    }

    Ok((cleaning_futs, output))
}

enum StaleFinderResult {
    CleanPath(ProjectRelativePathBuf),
    CleanChildren(Vec<ProjectRelativePathBuf>),
    CleanNone,
}

impl StaleFinderResult {
    fn clean_all(&self) -> bool {
        if let StaleFinderResult::CleanPath(_) = self {
            true
        } else {
            false
        }
    }

    fn paths(self) -> Vec<ProjectRelativePathBuf> {
        match self {
            StaleFinderResult::CleanPath(path) => vec![path],
            StaleFinderResult::CleanChildren(paths) => paths,
            StaleFinderResult::CleanNone => Vec::new(),
        }
    }
}

struct StaleFinderStats {
    stale_count: u64,
    retained_count: u64,
    untracked_count: u64,
    stale_bytes: u64,
    retained_bytes: u64,
    untracked_bytes: u64,
}

impl StaleFinderStats {
    fn new() -> Self {
        Self {
            stale_count: 0,
            retained_count: 0,
            untracked_count: 0,
            stale_bytes: 0,
            retained_bytes: 0,
            untracked_bytes: 0,
        }
    }
}

/// Get file size or directory size, without following symlinks
pub fn get_size(path: &AbsNormPath) -> anyhow::Result<u64> {
    let mut result = 0;
    if path.is_dir() {
        for entry in fs_util::read_dir(path)? {
            result += get_size(&entry?.path())?;
        }
    } else {
        result = path.symlink_metadata()?.len();
    }
    Ok(result)
}

fn find_stale_recursive(
    fs: &ProjectRoot,
    subtree: Option<&ArtifactTree>,
    path: &AbsNormPath,
    keep_since_time: DateTime<Utc>,
    stats: &mut StaleFinderStats,
) -> anyhow::Result<StaleFinderResult> {
    // Use symlink_metadata to not follow symlinks (stale/untracked symlink target may have been cleaned first)
    let path_type = FileType::from(path.symlink_metadata()?.file_type());
    let rel_path = fs.relativize(&path)?;

    let clean_untracked = |stats: &mut StaleFinderStats| {
        stats.untracked_count += 1;
        stats.untracked_bytes += get_size(path)?;
        tracing::trace!(path = %path, path_type = ?path_type, "marking as untracked");
        Ok(StaleFinderResult::CleanPath(rel_path.clone().into_owned()))
    };

    if let Some(DataTree::Data(metadata)) = subtree {
        if let ArtifactMaterializationStage::Materialized {
            last_access_time,
            active,
            ..
        } = metadata.stage
        {
            if last_access_time < keep_since_time && !active {
                stats.stale_count += 1;
                stats.stale_bytes += get_size(path)?;
                tracing::trace!(path = %path, path_type = ?path_type, "marking as stale");
                Ok(StaleFinderResult::CleanPath(rel_path.clone().into_owned()))
            } else {
                stats.retained_count += 1;
                stats.retained_bytes += get_size(path)?;
                tracing::trace!(path = %path, path_type = ?path_type, "marking as retained");
                Ok(StaleFinderResult::CleanNone)
            }
        } else {
            // Artifact was declared but never materialized, should not be a file here.
            clean_untracked(stats)
        }
    } else if path_type.is_dir() {
        let children = subtree.and_then(|t| t.children());
        let mut children_to_clean = Vec::new();
        let mut clean_dir = true;
        for file in fs_util::read_dir(path)? {
            let child_path = file?.path();
            // If a file or dir exists here but the name not valid utf-8 it must be untracked.
            // Make subtree None to mark it untracked instead of throwing an error.
            let file_name = child_path
                .file_name()
                .and_then(|f| f.to_str())
                .and_then(|f| FileName::new(f).ok());
            let subtree = children.and_then(|c| file_name.and_then(|f| c.get(f)));
            let child_result =
                find_stale_recursive(fs, subtree, &child_path, keep_since_time, stats)?;

            if !child_result.clean_all() {
                clean_dir = false;
            }
            children_to_clean.extend(child_result.paths());
        }
        // If all children should be cleaned, remove this dir instead
        if clean_dir {
            Ok(StaleFinderResult::CleanPath(rel_path.clone().into_owned()))
        } else {
            Ok(StaleFinderResult::CleanChildren(children_to_clean))
        }
    } else {
        clean_untracked(stats)
    }
}

fn find_stale_tracked_only(
    tree: &ArtifactTree,
    keep_since_time: DateTime<Utc>,
    stats: &mut StaleFinderStats,
) -> anyhow::Result<StaleFinderResult> {
    let mut paths_to_clean = Vec::new();
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
                stats.stale_count += 1;
                paths_to_clean.push(path);
            } else {
                tracing::trace!(path = %path, "retaining artifact");
                stats.retained_count += 1;
            }
        }
    }
    Ok(StaleFinderResult::CleanChildren(paths_to_clean))
}
