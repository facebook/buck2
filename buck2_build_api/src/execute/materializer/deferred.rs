/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{collections::HashSet, str::FromStr, sync::Arc};

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::file_ops::FileDigest;
use buck2_core::{
    directory::{unordered_entry_walk, DirectoryEntry},
    env_helper::EnvHelper,
    fs::{
        paths::{AbsPathBuf, RelativePathBuf},
        project::{ProjectFilesystem, ProjectRelativePath, ProjectRelativePathBuf},
    },
    result::SharedError,
};
use derive_more::Display;
use futures::{
    future::{BoxFuture, FutureExt, Shared, TryFutureExt},
    stream::{BoxStream, FuturesOrdered, StreamExt},
};
use gazebo::prelude::*;
use remote_execution::{NamedDigest, NamedDigestWithPermissions, REClientError, TCode, TDigest};
use thiserror::Error;
use tokio::{
    sync::{mpsc, oneshot},
    task::JoinHandle,
};
use tracing::instrument;

use crate::{
    actions::{
        artifact::ArtifactValue,
        artifact_utils::materialize_files,
        digest::FileDigestReExt,
        directory::{
            ActionDirectory, ActionDirectoryEntry, ActionDirectoryMember, ActionSharedDirectory,
        },
    },
    execute::{
        blocking::BlockingExecutor,
        commands::re::manager::ReConnectionManager,
        materializer::{
            filetree::{DataTreeEntry, FileTree},
            http::{http_client, http_download},
            io::MaterializeTreeStructure,
            ArtifactNotMaterializedReason, CasDownloadInfo, CopiedArtifact, HttpDownloadInfo,
            MaterializationError, Materializer,
        },
    },
};

/// Materializer implementation that defers materialization of declared
/// artifacts until they are needed (i.e. `ensure_materialized` is called).
///
/// # Important
///
/// This materializer defers both CAS fetches and local copies. Therefore, one
/// needs to be careful when choosing to call `ensure_materialized`.
/// Between `declare` and `ensure` calls, the local files could have changed.
///
/// This limits us to only "safely" using the materializer within the
/// computation of a build rule, and only to materialize inputs or outputs of
/// the rule, not random artifacts/paths. That's because:
/// - file changes before/after a build are handled by DICE, which invalidates
///   the outputs that depend on it. The materializer ends up having the wrong
///   information about these outputs. But because it's only used within the
///   build rules, the affected rule is recomputed and therefore has its
///   artifacts re-declared. So when `ensure` is called the materializer has
///   up-to-date information about the artifacts.
/// - file changes during a build are not properly supported by Buck and
///   treated as undefined behaviour, so there's no need to worry about them.
pub struct DeferredMaterializer {
    /// Sender to emit commands to the command loop. See `MaterializerCommand`.
    command_sender: mpsc::UnboundedSender<MaterializerCommand>,
    /// Handle of the command loop thread. Aborted on Drop.
    /// This thread serves as a queue for declare/ensure requests, making
    /// sure only one executes at a time and in the order they came in.
    /// TODO(rafaelc): aim to replace it with a simple mutex.
    command_thread: JoinHandle<()>,
    /// Determines what to do on `try_materialize_final_artifact`: if true,
    /// materializes them, otherwise skips them.
    materialize_final_artifacts: bool,
}

impl Drop for DeferredMaterializer {
    fn drop(&mut self) {
        // TODO(rafaelc): abort ongoing materialization futures?
        self.command_thread.abort();
    }
}

struct DeferredMaterializerCommandProcessor {
    project_root: AbsPathBuf,
    re_client_manager: Arc<ReConnectionManager>,
    /// Executor for blocking IO operations
    io_executor: Arc<dyn BlockingExecutor>,
    /// Used to emit MaterializationFinished to the command thread
    command_sender: mpsc::UnboundedSender<MaterializerCommand>,
}

// NOTE: This doesn't derive `Error` and that's on purpose.  We don't want to make it easy (or
// possible, in fact) to add  `context` to this SharedMaterializationError and lose the variant.
#[derive(Debug, Clone, Dupe)]
enum SharedMaterializationError {
    Error(SharedError),
    NotFound { info: Arc<CasDownloadInfo> },
}

#[derive(Error, Debug)]
enum MaterializeEntryError {
    #[error(transparent)]
    Error(#[from] anyhow::Error),

    /// The artifact wasn't found. This typically means it expired in the CAS.
    #[error("Artifact not found: declared by action {}", .info)]
    NotFound { info: Arc<CasDownloadInfo> },
}

impl From<MaterializeEntryError> for SharedMaterializationError {
    fn from(e: MaterializeEntryError) -> SharedMaterializationError {
        match e {
            MaterializeEntryError::Error(e) => Self::Error(e.into()),
            MaterializeEntryError::NotFound { info } => Self::NotFound { info },
        }
    }
}

type MaterializationFuture = Shared<BoxFuture<'static, Result<(), SharedMaterializationError>>>;

/// Message taken by the `DeferredMaterializer`'s command loop.
enum MaterializerCommand {
    // [Materializer trait methods -> Command thread]
    /// Takes a list of file paths, computes the materialized file paths of all
    /// of them, and sends the result through the oneshot.
    /// See `Materializer::get_materialized_file_paths` for more information.
    GetMaterializedFilePaths(
        Vec<ProjectRelativePathBuf>,
        oneshot::Sender<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>>,
    ),

    /// Declares an artifact: its path, value, and how to materialize it.
    Declare(
        ProjectRelativePathBuf,
        ArtifactValue,
        Box<ArtifactMaterializationMethod>, // Boxed to avoid growing all variants
    ),

    /// Declares that a given path is no longer eligible to be materialized by this materializer.
    /// This typically should reflect a change made to the underlying filesystem, either because
    /// the file was created, or because it was removed..
    InvalidateFilePath(ProjectRelativePathBuf),

    /// Takes a list of artifact paths, and materializes all artifacts in the
    /// list that have been declared but not yet been materialized. When the
    /// materialization starts, a future is sent back through the provided
    /// Sender; this future will be resolved when the materialization
    /// concludes (whether successfuly or not).
    Ensure(
        Vec<ProjectRelativePathBuf>,
        oneshot::Sender<BoxStream<'static, Result<(), MaterializationError>>>,
    ),

    /// [Materialization task -> Command thread]
    /// Notifies the command thread that an artifact was materialized. It takes
    /// the artifact path and the version that was materialized, such that if
    /// a newer version was declared during materialization - which should not
    /// happen under normal conditions - we can react accordingly.
    MaterializationFinished {
        path: ProjectRelativePathBuf,
        version: u64,
        result: Result<(), SharedMaterializationError>,
        has_deps: bool,
    },
}

impl std::fmt::Debug for MaterializerCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaterializerCommand::GetMaterializedFilePaths(paths, _) => {
                write!(f, "GetMaterializedFilePaths({:?}, _)", paths,)
            }
            MaterializerCommand::Declare(path, value, method) => {
                write!(f, "Declare({:?}, {:?}, {:?})", path, value, method,)
            }
            MaterializerCommand::InvalidateFilePath(path) => {
                write!(f, "InvalidateFilePath({:?})", path)
            }
            MaterializerCommand::Ensure(paths, _) => write!(f, "Ensure({:?}, _)", paths,),
            MaterializerCommand::MaterializationFinished {
                path,
                version,
                result,
                has_deps,
            } => f
                .debug_struct("MaterializationFinished")
                .field("path", path)
                .field("version", version)
                .field("result", result)
                .field("has_deps", has_deps)
                .finish(),
        }
    }
}

/// Tree that stores materialization data for each artifact. Used internally by
/// the `DeferredMaterializer` to keep track of artifacts and how to
/// materialize them.
type ArtifactTree = FileTree<Box<ArtifactMaterializationData>>;

struct ArtifactMaterializationData {
    value: ArtifactValue,
    method: Arc<ArtifactMaterializationMethod>,
    stage: ArtifactMaterializationStage,
    /// The version is just an internal counter that increases every time an
    /// artifact is declared (i.e. it tells us in which order they were declared).
    version: u64,
}

enum ArtifactMaterializationStage {
    /// The artifact was declared, but the materialization hasn't started yet.
    /// If it did start but end with an error, it returns to this stage.
    Declared,
    /// The materialization has started and it hasn't finished yet.
    Processing(MaterializationFuture),
    /// This artifact was materialized, but it needs to have its dependencies checked as they might
    /// have changed
    CheckDeps,
}

/// Different ways to materialize the files of an artifact. Some artifacts need
/// to be fetched from the CAS, others copied locally.
#[derive(Debug, Display)]
enum ArtifactMaterializationMethod {
    /// The files must be copied from a local path.
    ///
    /// The first argument is a map `[dest => src]`, meaning that a file at
    /// `{artifact_path}/{dest}/{p}` needs to be copied from `{src}/{p}`.
    ///
    /// The second argument is the raw list of copied artifacts, as received
    /// in `declare_copy`.
    #[display(fmt = "local copy")]
    LocalCopy(FileTree<ProjectRelativePathBuf>, Vec<CopiedArtifact>),

    /// The files must be fetched from the CAS.
    #[display(fmt = "cas download (action: {})", .info)]
    CasDownload {
        /// The digest of the action that produced this output
        info: Arc<CasDownloadInfo>,
    },

    /// The file must be fetched over HTTP.
    #[display(fmt = "http download ({})", info)]
    HttpDownload { info: HttpDownloadInfo },
}

#[async_trait]
impl Materializer for DeferredMaterializer {
    async fn declare_copy(
        &self,
        path: &ProjectRelativePath,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> anyhow::Result<()> {
        // TODO(rafaelc): get rid of this tree; it'd save a lot of memory.
        let mut srcs_tree = FileTree::new();
        for copied_artifact in srcs.iter() {
            let dest = copied_artifact.dest.strip_prefix(path)?;

            {
                let mut walk = unordered_entry_walk(copied_artifact.dest_entry.as_ref());
                while let Some((path, entry)) = walk.next() {
                    if let DirectoryEntry::Leaf(ActionDirectoryMember::File(..)) = entry {
                        let path = path.get();
                        let dest_iter = dest.iter().chain(path.iter()).map(|f| f.to_owned());
                        let src = if path.as_str().is_empty() {
                            copied_artifact.src.clone()
                        } else {
                            copied_artifact.src.join_unnormalized(&path)
                        };
                        srcs_tree.insert(dest_iter, src);
                    }
                }
            }
        }
        let cmd = MaterializerCommand::Declare(
            path.to_buf(),
            value,
            box ArtifactMaterializationMethod::LocalCopy(srcs_tree, srcs),
        );
        self.command_sender.send(cmd)?;
        Ok(())
    }

    async fn declare_cas_many<'a, 'b>(
        &self,
        info: Arc<CasDownloadInfo>,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        for (path, value) in artifacts {
            let cmd = MaterializerCommand::Declare(
                path,
                value,
                box ArtifactMaterializationMethod::CasDownload { info: info.dupe() },
            );
            self.command_sender.send(cmd)?;
        }
        Ok(())
    }

    async fn declare_http(
        &self,
        path: &ProjectRelativePath,
        info: HttpDownloadInfo,
    ) -> anyhow::Result<()> {
        let cmd = MaterializerCommand::Declare(
            path.to_owned(),
            ArtifactValue::file(info.metadata.dupe()),
            box ArtifactMaterializationMethod::HttpDownload { info },
        );
        self.command_sender.send(cmd)?;

        Ok(())
    }

    async fn invalidate_many(&self, paths: Vec<ProjectRelativePathBuf>) -> anyhow::Result<()> {
        for path in paths {
            self.command_sender
                .send(MaterializerCommand::InvalidateFilePath(path))?;
        }

        Ok(())
    }

    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<BoxStream<'static, Result<(), MaterializationError>>> {
        // TODO: display [materializing] in superconsole
        let (sender, recv) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Ensure(artifact_paths, sender))
            .context("Sending Ensure() command.")?;
        let materialization_fut = recv
            .await
            .context("Recv'ing materialization future from command thread.")?;
        Ok(materialization_fut)
    }

    async fn try_materialize_final_artifact(
        &self,
        artifact_path: ProjectRelativePathBuf,
    ) -> anyhow::Result<bool> {
        if self.materialize_final_artifacts {
            self.ensure_materialized(vec![artifact_path]).await?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    async fn get_materialized_file_paths(
        &self,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>> {
        if paths.is_empty() {
            return Ok(Vec::new());
        }
        let (sender, recv) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::GetMaterializedFilePaths(paths, sender))?;
        Ok(recv.await?)
    }
}

impl DeferredMaterializer {
    /// Spawns two threads (`materialization_loop` and `command_loop`).
    /// Creates and returns a new `DeferredMaterializer` that aborts those
    /// threads when dropped.
    pub fn new(
        project_root: AbsPathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        io_executor: Arc<dyn BlockingExecutor>,
        materialize_final_artifacts: bool,
    ) -> Self {
        let (command_sender, command_recv) = mpsc::unbounded_channel();

        let command_processor = Arc::new(DeferredMaterializerCommandProcessor {
            project_root,
            re_client_manager,
            io_executor,
            command_sender: command_sender.clone(),
        });
        let command_thread = tokio::spawn(async move { command_processor.run(command_recv).await });

        Self {
            command_sender,
            command_thread,
            materialize_final_artifacts,
        }
    }
}

impl DeferredMaterializerCommandProcessor {
    /// Loop that runs for as long as the materializer is alive.
    ///
    /// It takes commands via the `Materializer` trait methods.
    async fn run(self: Arc<Self>, mut command_recv: mpsc::UnboundedReceiver<MaterializerCommand>) {
        let mut tree = ArtifactTree::new();

        // Each Declare bumps the version, so that if an artifact is declared
        // a second time mid materialization of its previous version, we don't
        // incorrectly assume we materialized the latest version.
        let mut next_version = 0u64;

        while let Some(command) = command_recv.recv().await {
            match command {
                // Entry point for `get_materialized_file_paths` calls
                MaterializerCommand::GetMaterializedFilePaths(paths, result_sender) => {
                    let result = paths.into_map(|p| tree.file_contents_path(p));
                    result_sender.send(result).ok();
                }
                // Entry point for `declare_{copy|cas}` calls
                MaterializerCommand::Declare(path, value, method) => {
                    // TODO(rafaelc): consider what to do if an artifact is materializing
                    // and we declare it again: abort the materialization? Err? Panic?
                    tracing::trace!(
                        path = %path,
                        method = %method,
                        value = %value.entry(),
                        version = next_version,
                        "declare artifact",
                    );
                    let data = box ArtifactMaterializationData {
                        value,
                        method: Arc::new(*method),
                        stage: ArtifactMaterializationStage::Declared,
                        version: next_version,
                    };
                    next_version += 1;
                    tree.insert(path.iter().map(|f| f.to_owned()), data);
                }
                MaterializerCommand::InvalidateFilePath(path) => {
                    tracing::trace!(
                        path = %path,
                        "invalidate path",
                    );
                    tree.remove(path.iter());
                }
                // Entry point for `ensure_materialized` calls
                MaterializerCommand::Ensure(paths, fut_sender) => {
                    fut_sender
                        .send(self.dupe().materialize_many_artifacts(&mut tree, paths))
                        .ok();
                }
                // Materialization of artifact succeeded
                MaterializerCommand::MaterializationFinished {
                    path,
                    version,
                    result,
                    has_deps,
                } => {
                    tree.materialization_finished(path, version, result, has_deps);
                }
            }
        }
    }

    fn materialize_many_artifacts(
        self: Arc<Self>,
        tree: &mut ArtifactTree,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> BoxStream<'static, Result<(), MaterializationError>> {
        let tasks = paths.into_iter().filter_map(|path| {
            self.dupe()
                .materialize_artifact(tree, path.as_ref())
                .map(move |fut| {
                    fut.map_err(move |e| match e {
                        SharedMaterializationError::Error(source) => MaterializationError::Error {
                            path,
                            source: source.into(),
                        },
                        SharedMaterializationError::NotFound { info } => {
                            MaterializationError::NotFound { path, info }
                        }
                    })
                })
        });

        tasks.collect::<FuturesOrdered<_>>().boxed()
    }

    #[instrument(level = "debug", skip(self, tree), fields(path = %path))]
    fn materialize_artifact(
        self: Arc<Self>,
        tree: &mut ArtifactTree,
        path: &ProjectRelativePath,
    ) -> Option<MaterializationFuture> {
        // Get the data about the artifact, or return early if processing/processed
        let (entry, deps, method, version) = match tree.get(path.iter()) {
            // Already processed, or never declared, nothing to do
            None => {
                tracing::debug!("not known");
                return None;
            }
            Some(data) => match &data.stage {
                ArtifactMaterializationStage::Processing(fut) => {
                    tracing::debug!("join existing future");
                    return Some(fut.clone());
                }
                ArtifactMaterializationStage::Declared => (
                    Some(data.value.entry().dupe()),
                    data.value.deps().duped(),
                    data.method.dupe(),
                    data.version,
                ),
                ArtifactMaterializationStage::CheckDeps => (
                    None,
                    data.value.deps().duped(),
                    data.method.dupe(),
                    data.version,
                ),
            },
        };

        tracing::debug!(
            method = %method,
            has_entry = entry.is_some(),
            has_deps = deps.is_some(),
            version = version,
            "materialize artifact"
        );

        // If the artifact copies from other artifacts, we must materialize them first
        let deps_tasks = match method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { .. }
            | ArtifactMaterializationMethod::HttpDownload { .. } => Vec::new(),
            ArtifactMaterializationMethod::LocalCopy(_, copied_artifacts) => copied_artifacts
                .iter()
                .filter_map(|a| self.dupe().materialize_artifact(tree, a.src.as_ref()))
                .collect::<Vec<_>>(),
        };

        // The artifact might have symlinks pointing to other artifacts. We must
        // materialize them as well, to avoid dangling synlinks.
        let link_deps_tasks = match deps.as_ref() {
            None => Vec::new(),
            Some(deps) => tree
                .find_artifacts(deps)
                .into_iter()
                .filter_map(|p| self.dupe().materialize_artifact(tree, p.as_ref()))
                .collect::<Vec<_>>(),
        };

        // Create a task to await deps and materialize ourselves
        let path_buf = path.to_buf();
        let path_buf_dup = path_buf.clone();
        let command_sender = self.command_sender.clone();
        let task = tokio::task::spawn(async move {
            // Materialize the deps and this entry. This *must* happen in a try block because we
            // need to notity the materializer regardless of whether this succeeds or fails.

            let res: Result<(), SharedMaterializationError> = try {
                // In case this is a local copy, we first need to materialize the
                // artifacts we are copying from, before we can copy them.
                for t in deps_tasks {
                    t.await?;
                }

                // All potential deps are materialized. Now materialize the entry.
                if let Some(entry) = entry {
                    self.dupe()
                        .materialize_entry(path_buf.clone(), method, entry)
                        .await?;
                }

                // Wait for the deps (targets) of the entry's symlinks to be materialized
                for t in link_deps_tasks {
                    t.await?;
                }
            };

            // Materialization finished, notify the command thread
            let _ignored = command_sender.send(MaterializerCommand::MaterializationFinished {
                path: path_buf_dup,
                version,
                result: res.dupe(),
                has_deps: deps.is_some(),
            });

            res
        })
        .map(|r| match r {
            Ok(r) => r,
            Err(e) => Err(SharedMaterializationError::Error(e.into())), // Turn the JoinError into a SharedError.
        })
        .boxed()
        .shared();

        let data = tree.get_mut(path.iter()).unwrap();
        data.stage = ArtifactMaterializationStage::Processing(task.clone());

        Some(task)
    }

    /// Materializes an `entry` at `path`, using the materialization `method`
    #[instrument(level = "debug", skip(self), fields(path = %path, method = %method, entry = %entry))]
    async fn materialize_entry(
        self: Arc<Self>,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
    ) -> Result<(), MaterializeEntryError> {
        // Materialize the dir structure, and symlinks
        self.io_executor
            .execute_io(box MaterializeTreeStructure {
                path: path.clone(),
                entry: entry.dupe(),
            })
            .await?;

        // Materialize files
        match method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { info } => {
                let mut files = Vec::new();

                {
                    let mut walk = unordered_entry_walk(entry.as_ref());

                    while let Some((entry_path, entry)) = walk.next() {
                        if let DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) = entry {
                            let name = path.join_normalized(entry_path.get())?.to_string();
                            let digest = maybe_tombstone_digest(&f.digest)?.to_re();

                            tracing::trace!(name = %name, digest = %digest, "push download");

                            files.push(NamedDigestWithPermissions {
                                named_digest: NamedDigest {
                                    name,
                                    digest,
                                    ..Default::default()
                                },
                                is_executable: f.is_executable,
                                ..Default::default()
                            });
                        }
                    }
                }

                let connection = self.re_client_manager.get_re_connection();
                let re_client = connection.get_client();

                re_client.materialize_files(files).await.map_err(|e| {
                    match e.downcast_ref::<REClientError>() {
                        Some(e) if e.code == TCode::NOT_FOUND => {
                            MaterializeEntryError::NotFound { info: info.dupe() }
                        }
                        _ => MaterializeEntryError::Error(e.context({
                            format!("Error materializing files declared by action: {}", info)
                        })),
                    }
                })?;
            }
            ArtifactMaterializationMethod::HttpDownload { info } => {
                async {
                    let downloaded = http_download(
                        &http_client()?,
                        &ProjectFilesystem {
                            root: self.project_root.clone(),
                        },
                        &path,
                        &info.url,
                        &info.checksum,
                        info.metadata.is_executable,
                    )
                    .await?;

                    // Check that the size we got was the one that we expected. This isn't stricly
                    // speaking necessary here, but since an invalid size would break actions
                    // running on RE, it's a good idea to catch it here when materializing so that
                    // our test suite can surface bugs when downloading things locally.
                    if downloaded.size != info.metadata.digest.size {
                        return Err(anyhow::anyhow!(
                            "Downloaded size ({}) does not match expected size ({})",
                            downloaded.size,
                            info.metadata.digest.size,
                        ));
                    }

                    Ok(())
                }
                .await
                .with_context(|| {
                    format!(
                        "Error materializing HTTP resource declared by target `{}`",
                        info.owner
                    )
                })?;
            }
            ArtifactMaterializationMethod::LocalCopy(_, copied_artifacts) => {
                self.io_executor
                    .execute_io_inline(box || {
                        for a in copied_artifacts {
                            materialize_files(
                                a.dest_entry.as_ref(),
                                &self.project_root.join_unnormalized(&a.src),
                                &self.project_root.join_unnormalized(&a.dest),
                            )?;
                        }
                        Ok(())
                    })
                    .await?;
            }
        };
        Ok(())
    }
}

impl ArtifactTree {
    /// Given a path that's (possibly) not yet materialized, returns the path
    /// `contents_path` where its contents can be found.
    /// - If the contents aren't materialized anywhere yet, returns
    ///   `(contents_path, true)` meaning it'll be available in `contents_path`
    ///   after a CAS fetch is done on that path.
    /// - If the contents are already fetched, returns
    ///   `Some(contents_path, false)`.
    ///
    /// Note that the returned `contents_path` could be the same as `path`.
    #[instrument(level = "trace", skip(self), fields(path = %path))]
    fn file_contents_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason> {
        let mut path_iter = path.iter();
        let materialization_data = match self.prefix_get(&mut path_iter) {
            // not in the tree, assume it's materialized already since we
            // remove materialized paths from the tree
            None => return Ok(path),
            Some(data) => data,
        };
        match materialization_data.method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { info } => {
                let path_iter = path_iter.peekable();

                let mut entry = Some(
                    materialization_data
                        .value
                        .entry()
                        .as_ref()
                        .map_dir(|d| d as &dyn ActionDirectory),
                );

                // Check if the path we are asking for exists in this entry.
                for name in path_iter {
                    entry = match entry {
                        Some(DirectoryEntry::Dir(d)) => d.get(name),
                        _ => break,
                    }
                }

                match entry {
                    Some(entry) => Err(ArtifactNotMaterializedReason::RequiresCasDownload {
                        path,
                        // TODO (@torozco): A nicer API to get an Immutable directory here.
                        entry: entry
                            .map_dir(|d| d.to_builder().fingerprint())
                            .map_leaf(|l| l.dupe()),
                        info: info.dupe(),
                    }),
                    None => Err(
                        ArtifactNotMaterializedReason::DeferredMaterializerCorruption {
                            path,
                            entry: materialization_data.value.entry().dupe(),
                            info: info.dupe(),
                        },
                    ),
                }
            }
            ArtifactMaterializationMethod::HttpDownload { .. } => {
                Err(ArtifactNotMaterializedReason::RequiresHttpDownload { path })
            }
            // TODO: also record and check materialized_files for LocalCopy
            ArtifactMaterializationMethod::LocalCopy(srcs, _) => {
                match srcs.prefix_get(&mut path_iter) {
                    None => Ok(path),
                    Some(src_path) => match path_iter.next() {
                        None => self.file_contents_path(src_path.clone()),
                        // This is not supposed to be reachable, and if it's, there
                        // is a bug somewhere else. Panic to prevent the bug from
                        // propagating.
                        Some(part) => panic!(
                            "While getting materialized path of {:?}: path {:?} is a file, so subpath {:?} doesn't exist within.",
                            path, src_path, part,
                        ),
                    },
                }
            }
        }
    }

    #[instrument(level = "debug", skip(self, result), fields(path = %artifact_path, version = %version))]
    fn materialization_finished(
        &mut self,
        artifact_path: ProjectRelativePathBuf,
        version: u64,
        result: Result<(), SharedMaterializationError>,
        has_deps: bool,
    ) {
        let path = artifact_path.iter().map(|e| e.to_owned());

        match self.entry(path) {
            Some(DataTreeEntry::Occupied(mut info)) => {
                if info.get().version > version {
                    tracing::debug!("version conflict");
                    return;
                }

                if result.is_err() {
                    tracing::debug!("transition to Declared");
                    info.get_mut().stage = ArtifactMaterializationStage::Declared;
                    return;
                }

                if has_deps {
                    tracing::debug!("transition to CheckDeps");
                    info.get_mut().stage = ArtifactMaterializationStage::CheckDeps;
                    return;
                }

                // If we entry is a tree and we have a file,
                tracing::debug!("done");
                info.remove();
            }
            Some(DataTreeEntry::Vacant(..)) => {
                tracing::debug!("materialization_finished but path is vacant!")
            }
            None => {
                tracing::debug!("materialization_finished but path traverses through an entry!")
            }
        }
    }
}

impl<V> FileTree<V> {
    /// Finds all the paths in `deps` that are artifacts in `self`
    fn find_artifacts<D>(&self, deps: &D) -> Vec<ProjectRelativePathBuf>
    where
        D: ActionDirectory + ?Sized,
    {
        fn walk_deps<V, D>(
            tree: &FileTree<V>,
            entry: DirectoryEntry<&D, &ActionDirectoryMember>,
            path: &mut RelativePathBuf,
            found_artifacts: &mut Vec<ProjectRelativePathBuf>,
        ) where
            D: ActionDirectory + ?Sized,
        {
            match tree {
                FileTree::Data(_) => {
                    found_artifacts.push(ProjectRelativePathBuf::unchecked_new(path.to_string()));
                }
                FileTree::Tree(tree_children) => {
                    // Not an artifact, but if entry is a directory we can search deeper within
                    if let DirectoryEntry::Dir(d) = entry {
                        for (name, child) in d.entries() {
                            if let Some(subtree) = tree_children.get(name) {
                                path.push(name);
                                walk_deps(subtree, child, path, found_artifacts);
                                path.pop();
                            }
                        }
                    }
                }
            }
        }

        let mut artifacts = Vec::new();
        walk_deps(
            self,
            DirectoryEntry::Dir(deps),
            &mut RelativePathBuf::new(),
            &mut artifacts,
        );
        artifacts
    }
}

/// This is used for testing to ingest digests (via BUCK2_TEST_TOMBSTONED_DIGESTS).
fn maybe_tombstone_digest(digest: &FileDigest) -> anyhow::Result<&FileDigest> {
    // This has to be of size 1 since size 0 will result in the RE client just producing an empty
    // instead of a not-found error.
    static TOMBSTONE_DIGEST: FileDigest = FileDigest::new([0; 20], 1);

    fn convert_digests(val: &str) -> anyhow::Result<HashSet<FileDigest>> {
        val.split(' ')
            .map(|digest| {
                let digest = TDigest::from_str(digest)
                    .with_context(|| format!("Invalid digest: `{}`", digest))?;
                let digest = FileDigest::from_re(&digest);
                anyhow::Ok(digest)
            })
            .collect()
    }

    static TOMBSTONED_DIGESTS: EnvHelper<HashSet<FileDigest>> =
        EnvHelper::with_converter("BUCK2_TEST_TOMBSTONED_DIGESTS", convert_digests);

    if let Some(digests) = TOMBSTONED_DIGESTS.get()?.as_ref() {
        if digests.contains(digest) {
            return Ok(&TOMBSTONE_DIGEST);
        }
    }

    Ok(digest)
}

#[cfg(test)]
mod tests {
    use buck2_common::file_ops::FileMetadata;

    use super::*;
    use crate::actions::directory::{insert_file, ActionDirectoryBuilder};

    #[test]
    pub fn test_find_artifacts() -> anyhow::Result<()> {
        let artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/baz".to_owned());
        let artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/qux".to_owned());
        let artifact3 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/quux".to_owned());
        let artifact4 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux/quuz".to_owned());
        let non_artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux".to_owned());
        let non_artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/corge".to_owned());

        let file = FileMetadata::empty();

        // Build deps with artifacts 1-3, and non-artifacts 1-2
        let mut builder = ActionDirectoryBuilder::empty();
        insert_file(
            &mut builder,
            artifact1.join_normalized("f1")?.as_ref(),
            file.dupe(),
        )?;
        insert_file(
            &mut builder,
            artifact2.join_normalized("d/f1")?.as_ref(),
            file.dupe(),
        )?;
        insert_file(&mut builder, artifact3.as_ref(), file.dupe())?;
        insert_file(&mut builder, non_artifact2.as_ref(), file.dupe())?;
        builder.mkdir(&non_artifact1)?;

        // Build tree with artifacts 1-4
        let mut tree: FileTree<()> = FileTree::new();
        tree.insert(artifact1.iter().map(|f| f.to_owned()), ());
        tree.insert(artifact2.iter().map(|f| f.to_owned()), ());
        tree.insert(artifact3.iter().map(|f| f.to_owned()), ());
        tree.insert(artifact4.iter().map(|f| f.to_owned()), ());

        let expected_artifacts: HashSet<_> =
            vec![artifact1, artifact2, artifact3].into_iter().collect();
        let found_artifacts: HashSet<_> = tree.find_artifacts(&builder).into_iter().collect();
        assert_eq!(found_artifacts, expected_artifacts);
        Ok(())
    }
}
