/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod extension;
mod file_tree;

use std::collections::HashSet;
use std::collections::VecDeque;
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::result::SharedError;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DirectoryEntry;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestFromReExt;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::directory::ActionDirectory;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::materialize::http::http_client;
use buck2_execute::materialize::http::http_download;
use buck2_execute::materialize::materializer::ArtifactNotMaterializedReason;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::CopiedArtifact;
use buck2_execute::materialize::materializer::DeclareMatchOutcome;
use buck2_execute::materialize::materializer::DeferredMaterializerExtensions;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use buck2_execute::materialize::materializer::MaterializationError;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_execute::output_size::OutputSize;
use buck2_execute::re::manager::ReConnectionManager;
use chrono::Duration;
use chrono::Utc;
use derive_more::Display;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use futures::future::TryFutureExt;
use futures::stream::BoxStream;
use futures::stream::FuturesOrdered;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use once_cell::sync::Lazy;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;
use remote_execution::REClientError;
use remote_execution::TCode;
use remote_execution::TDigest;
use thiserror::Error;
use tokio::sync::mpsc;
use tokio::sync::oneshot;
use tokio::task::JoinHandle;
use tokio_stream::wrappers::IntervalStream;
use tokio_stream::wrappers::UnboundedReceiverStream;
use tracing::instrument;

use crate::materializers::deferred::extension::ExtensionCommand;
use crate::materializers::deferred::file_tree::DataTreeIntoIterator;
use crate::materializers::deferred::file_tree::DataTreeIterator;
use crate::materializers::deferred::file_tree::FileTree;
use crate::materializers::immediate;
use crate::materializers::io::materialize_files;
use crate::materializers::io::MaterializeTreeStructure;
use crate::materializers::sqlite::MaterializerState;
use crate::materializers::sqlite::MaterializerStateSqliteDb;

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
#[derive(Allocative)]
pub struct DeferredMaterializer {
    /// Sender to emit commands to the command loop. See `MaterializerCommand`.
    #[allocative(skip)]
    command_sender: mpsc::UnboundedSender<MaterializerCommand>,
    /// Handle of the command loop thread. Aborted on Drop.
    /// This thread serves as a queue for declare/ensure requests, making
    /// sure only one executes at a time and in the order they came in.
    /// TODO(rafaelc): aim to replace it with a simple mutex.
    #[allocative(skip)]
    command_thread: JoinHandle<()>,
    /// Determines what to do on `try_materialize_final_artifact`: if true,
    /// materializes them, otherwise skips them.
    materialize_final_artifacts: bool,
    defer_write_actions: bool,

    /// To be removed, used to implement write for now.
    fs: ProjectRoot,
    io_executor: Arc<dyn BlockingExecutor>,
}

impl Drop for DeferredMaterializer {
    fn drop(&mut self) {
        // TODO(rafaelc): abort ongoing materialization futures?
        self.command_thread.abort();
    }
}

pub struct DeferredMaterializerConfigs {
    pub materialize_final_artifacts: bool,
    pub defer_write_actions: bool,
    pub ttl_refresh_frequency: std::time::Duration,
    pub ttl_refresh_min_ttl: Duration,
    pub ttl_refresh_enabled: bool,
}

struct DeferredMaterializerCommandProcessor {
    fs: ProjectRoot,
    re_client_manager: Arc<ReConnectionManager>,
    /// Executor for blocking IO operations
    io_executor: Arc<dyn BlockingExecutor>,
    /// Used to emit MaterializationFinished to the command thread
    command_sender: mpsc::UnboundedSender<MaterializerCommand>,
    sqlite_db: Option<Arc<MaterializerStateSqliteDb>>,
    ttl_refresh_frequency: std::time::Duration,
    ttl_refresh_min_ttl: Duration,
    ttl_refresh_enabled: bool,
}

struct MaterializationStat {
    file_count: u64,
    total_bytes: u64,
}

// NOTE: This doesn't derive `Error` and that's on purpose.  We don't want to make it easy (or
// possible, in fact) to add  `context` to this SharedProcessingError and lose the variant.
#[derive(Debug, Clone, Dupe)]
enum SharedMaterializingError {
    Error(SharedError),
    NotFound { info: Arc<CasDownloadInfo> },
    SqliteDbError(SharedError),
}

#[derive(Error, Debug)]
enum MaterializeEntryError {
    #[error(transparent)]
    Error(#[from] anyhow::Error),

    /// The artifact wasn't found. This typically means it expired in the CAS.
    #[error("Artifact not found: declared by action {}", .info)]
    NotFound { info: Arc<CasDownloadInfo> },
}

impl From<MaterializeEntryError> for SharedMaterializingError {
    fn from(e: MaterializeEntryError) -> SharedMaterializingError {
        match e {
            MaterializeEntryError::Error(e) => Self::Error(e.into()),
            MaterializeEntryError::NotFound { info } => Self::NotFound { info },
        }
    }
}

/// A future that is materializing on a separate task spawned by the materializer
type MaterializingFuture = Shared<BoxFuture<'static, Result<(), SharedMaterializingError>>>;
/// A future that is cleaning paths on a separate task spawned by the materializer
type CleaningFuture = Shared<BoxFuture<'static, SharedResult<()>>>;

#[derive(Clone)]
enum ProcessingFuture {
    Materializing(MaterializingFuture),
    Cleaning(CleaningFuture),
}

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

    /// Declares that a set of artifacts already exist
    DeclareExisting(Vec<(ProjectRelativePathBuf, ArtifactValue)>),

    /// Declares an artifact: its path, value, and how to materialize it.
    Declare(
        ProjectRelativePathBuf,
        ArtifactValue,
        Box<ArtifactMaterializationMethod>, // Boxed to avoid growing all variants
    ),

    MatchArtifacts(
        Vec<(ProjectRelativePathBuf, ArtifactValue)>,
        oneshot::Sender<bool>,
    ),

    /// Declares that given paths are no longer eligible to be materialized by this materializer.
    /// This typically should reflect a change made to the underlying filesystem, either because
    /// the file was created, or because it was removed..
    InvalidateFilePaths(Vec<ProjectRelativePathBuf>, oneshot::Sender<CleaningFuture>),

    /// Takes a list of artifact paths, and materializes all artifacts in the
    /// list that have been declared but not yet been materialized. When the
    /// materialization starts, a future is sent back through the provided
    /// Sender; this future will be resolved when the materialization
    /// concludes (whether successfuly or not).
    Ensure(
        Vec<ProjectRelativePathBuf>,
        EventDispatcher,
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
        result: Result<(), SharedMaterializingError>,
    },

    Extension(Box<dyn ExtensionCommand>),
}

impl std::fmt::Debug for MaterializerCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaterializerCommand::GetMaterializedFilePaths(paths, _) => {
                write!(f, "GetMaterializedFilePaths({:?}, _)", paths,)
            }
            MaterializerCommand::DeclareExisting(paths) => {
                write!(f, "GetMaterializedFilePaths({:?})", paths,)
            }
            MaterializerCommand::Declare(path, value, method) => {
                write!(f, "Declare({:?}, {:?}, {:?})", path, value, method,)
            }
            MaterializerCommand::MatchArtifacts(paths, _) => {
                write!(f, "MatchArtifacts({:?})", paths)
            }
            MaterializerCommand::InvalidateFilePaths(paths, _) => {
                write!(f, "InvalidateFilePaths({:?})", paths)
            }
            MaterializerCommand::Ensure(paths, _, _) => write!(f, "Ensure({:?}, _)", paths,),
            MaterializerCommand::MaterializationFinished {
                path,
                version,
                result,
            } => f
                .debug_struct("MaterializationFinished")
                .field("path", path)
                .field("version", version)
                .field("result", result)
                .finish(),
            MaterializerCommand::Extension(ext) => write!(f, "Extension({:?})", ext),
        }
    }
}

/// Tree that stores materialization data for each artifact. Used internally by
/// the `DeferredMaterializer` to keep track of artifacts and how to
/// materialize them.
type ArtifactTree = FileTree<Box<ArtifactMaterializationData>>;

struct ArtifactMaterializationData {
    /// Taken from `deps` of `ArtifactValue`. Used to materialize deps of the artifact.
    deps: Option<ActionSharedDirectory>,
    stage: ArtifactMaterializationStage,
    /// The version is just an internal counter that increases every time an
    /// artifact is declared (i.e. it tells us in which order they were declared).
    version: u64,
    /// An optional future that may be processing something at the current path
    /// (for example, materializing or deleting). Any other future that needs to process
    /// this path would need to wait on the existing future to finish.
    /// TODO(scottcao): Turn this into a queue of pending futures.
    processing_fut: Option<ProcessingFuture>,
}

/// Fingerprint used to identify `ActionSharedDirectory`. We give it an explicit
/// alias because `TrackedFileDigest` can look confusing.
pub type ActionDirectoryFingerprint = TrackedFileDigest;

/// Metadata used to identify an artifact entry without all of its content. Stored on materialized
/// artifacts to check matching artifact optimizations. For `ActionSharedDirectory`, we use its fingerprint,
/// For everything else (files, symlinks, and external symlinks), we use `ActionDirectoryMember`
/// as is because it already holds the metadata we need.
#[derive(Clone, Dupe, Debug, PartialEq, Eq)]
pub struct ArtifactMetadata(pub ActionDirectoryEntry<ActionDirectoryFingerprint>);

impl From<ActionDirectoryEntry<ActionSharedDirectory>> for ArtifactMetadata {
    fn from(entry: ActionDirectoryEntry<ActionSharedDirectory>) -> Self {
        let new_entry: ActionDirectoryEntry<ActionDirectoryFingerprint> = match entry {
            DirectoryEntry::Dir(dir) => DirectoryEntry::Dir(dir.fingerprint().dupe()),
            DirectoryEntry::Leaf(leaf) => DirectoryEntry::Leaf(leaf),
        };
        Self(new_entry)
    }
}

enum ArtifactMaterializationStage {
    /// The artifact was declared, but the materialization hasn't started yet.
    /// If it did start but end with an error, it returns to this stage.
    /// When the the artifact was declared, we spawn a deletion future to delete
    /// all existing paths that conflict with the output paths.
    Declared {
        /// Taken from `entry` of `ArtifactValue`. Used to materialize the actual artifact.
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        method: Arc<ArtifactMaterializationMethod>,
    },
    /// This artifact was materialized
    Materialized {
        /// Once the artifact is materialized, we don't need the full entry anymore.
        /// We can throw away most of the entry and just keep some metadata used to
        /// check if materialized artifact matches declared artifact.
        metadata: ArtifactMetadata,
    },
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

    #[display(fmt = "write")]
    Write {
        compressed_data: Box<[u8]>,
        decompressed_size: usize,
        is_executable: bool,
    },

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

trait MaterializationMethodToProto {
    fn to_proto(&self) -> buck2_data::MaterializationMethod;
}

impl MaterializationMethodToProto for ArtifactMaterializationMethod {
    fn to_proto(&self) -> buck2_data::MaterializationMethod {
        match self {
            ArtifactMaterializationMethod::LocalCopy { .. } => {
                buck2_data::MaterializationMethod::LocalCopy
            }
            ArtifactMaterializationMethod::CasDownload { .. } => {
                buck2_data::MaterializationMethod::CasDownload
            }
            ArtifactMaterializationMethod::Write { .. } => buck2_data::MaterializationMethod::Write,
            ArtifactMaterializationMethod::HttpDownload { .. } => {
                buck2_data::MaterializationMethod::HttpDownload
            }
        }
    }
}

#[async_trait]
impl Materializer for DeferredMaterializer {
    async fn declare_existing(
        &self,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        let cmd = MaterializerCommand::DeclareExisting(artifacts);
        self.command_sender.send(cmd)?;
        Ok(())
    }

    async fn declare_copy_impl(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> anyhow::Result<()> {
        // TODO(rafaelc): get rid of this tree; it'd save a lot of memory.
        let mut srcs_tree = FileTree::new();
        for copied_artifact in srcs.iter() {
            let dest = copied_artifact.dest.strip_prefix(&path)?;

            {
                let mut walk = unordered_entry_walk(copied_artifact.dest_entry.as_ref());
                while let Some((path, entry)) = walk.next() {
                    if let DirectoryEntry::Leaf(ActionDirectoryMember::File(..)) = entry {
                        let path = path.get();
                        let dest_iter = dest.iter().chain(path.iter()).map(|f| f.to_owned());
                        let src = if path.as_str().is_empty() {
                            copied_artifact.src.clone()
                        } else {
                            copied_artifact.src.join(&path)
                        };
                        srcs_tree.insert(dest_iter, src);
                    }
                }
            }
        }
        let cmd = MaterializerCommand::Declare(
            path,
            value,
            box ArtifactMaterializationMethod::LocalCopy(srcs_tree, srcs),
        );
        self.command_sender.send(cmd)?;
        Ok(())
    }

    async fn declare_cas_many_impl<'a, 'b>(
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
        path: ProjectRelativePathBuf,
        info: HttpDownloadInfo,
    ) -> anyhow::Result<()> {
        let cmd = MaterializerCommand::Declare(
            path,
            ArtifactValue::file(info.metadata.dupe()),
            box ArtifactMaterializationMethod::HttpDownload { info },
        );
        self.command_sender.send(cmd)?;

        Ok(())
    }

    async fn write<'a>(
        &self,
        gen: Box<dyn FnOnce() -> anyhow::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> anyhow::Result<Vec<ArtifactValue>> {
        if !self.defer_write_actions {
            return immediate::write_to_disk(&self.fs, self.io_executor.as_ref(), gen).await;
        }

        let contents = gen()?;

        let mut paths = Vec::with_capacity(contents.len());
        let mut values = Vec::with_capacity(contents.len());
        let mut methods = Vec::with_capacity(contents.len());

        for WriteRequest {
            path,
            content,
            is_executable,
        } in contents
        {
            let digest = FileDigest::from_bytes_sha1(&content);

            let meta = FileMetadata {
                digest: TrackedFileDigest::new(digest),
                is_executable,
            };

            // NOTE: The zstd crate doesn't release extra capacity of its encoding buffer so it's
            // important to do so here (or the compressed Vec is the same capacity as the input!).
            let compressed_data = zstd::bulk::compress(&content, 0)
                .with_context(|| format!("Error compressing {} bytes", content.len()))?
                .into_boxed_slice();

            paths.push(path);
            values.push(ArtifactValue::file(meta));
            methods.push(ArtifactMaterializationMethod::Write {
                compressed_data,
                decompressed_size: content.len(),
                is_executable,
            });
        }

        for (path, (value, method)) in std::iter::zip(
            paths.into_iter(),
            std::iter::zip(values.iter(), methods.into_iter()),
        ) {
            self.command_sender.send(MaterializerCommand::Declare(
                path,
                value.dupe(),
                box method,
            ))?;
        }

        Ok(values)
    }

    async fn declare_match(
        &self,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<DeclareMatchOutcome> {
        let (sender, recv) = oneshot::channel();

        self.command_sender
            .send(MaterializerCommand::MatchArtifacts(artifacts, sender))?;

        let is_match = recv
            .await
            .context("Recv'ing match future from command thread.")?;

        Ok(is_match.into())
    }

    async fn invalidate_many(&self, paths: Vec<ProjectRelativePathBuf>) -> anyhow::Result<()> {
        let (sender, recv) = oneshot::channel();

        self.command_sender
            .send(MaterializerCommand::InvalidateFilePaths(paths, sender))?;

        // Wait on future to finish before invalidation can continue.
        let invalidate_fut = recv.await?;
        invalidate_fut.await.unshared_error()
    }

    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<BoxStream<'static, Result<(), MaterializationError>>> {
        let event_dispatcher = get_dispatcher();

        // TODO: display [materializing] in superconsole
        let (sender, recv) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Ensure(
                artifact_paths,
                event_dispatcher,
                sender,
            ))
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

    fn as_deferred_materializer_extension(&self) -> Option<&dyn DeferredMaterializerExtensions> {
        Some(self as _)
    }
}

impl DeferredMaterializer {
    /// Spawns two threads (`materialization_loop` and `command_loop`).
    /// Creates and returns a new `DeferredMaterializer` that aborts those
    /// threads when dropped.
    pub fn new(
        fs: ProjectRoot,
        re_client_manager: Arc<ReConnectionManager>,
        io_executor: Arc<dyn BlockingExecutor>,
        configs: DeferredMaterializerConfigs,
        sqlite_db: Option<MaterializerStateSqliteDb>,
        sqlite_state: Option<MaterializerState>,
    ) -> Self {
        let (command_sender, command_recv) = mpsc::unbounded_channel();

        let command_processor = Arc::new(DeferredMaterializerCommandProcessor {
            fs: fs.dupe(),
            re_client_manager,
            io_executor: io_executor.dupe(),
            command_sender: command_sender.clone(),
            sqlite_db: sqlite_db.map(Arc::new),
            ttl_refresh_frequency: configs.ttl_refresh_frequency,
            ttl_refresh_min_ttl: configs.ttl_refresh_min_ttl,
            ttl_refresh_enabled: configs.ttl_refresh_enabled,
        });

        let mut tree = ArtifactTree::new();
        if let Some(sqlite_state) = sqlite_state {
            for (path, metadata) in sqlite_state.into_iter() {
                tree.insert(
                    path.iter().map(|f| f.to_owned()),
                    box ArtifactMaterializationData {
                        deps: None,
                        stage: ArtifactMaterializationStage::Materialized { metadata },
                        version: 0u64, // Any state restored from disk always gets set to version 0
                        processing_fut: None,
                    },
                );
            }
        }

        let command_thread =
            tokio::spawn(async move { command_processor.run(command_recv, tree).await });

        Self {
            command_sender,
            command_thread,
            materialize_final_artifacts: configs.materialize_final_artifacts,
            defer_write_actions: configs.defer_write_actions,
            fs,
            io_executor,
        }
    }
}

impl DeferredMaterializerCommandProcessor {
    /// Loop that runs for as long as the materializer is alive.
    ///
    /// It takes commands via the `Materializer` trait methods.
    async fn run(
        self: Arc<Self>,
        commands: mpsc::UnboundedReceiver<MaterializerCommand>,
        mut tree: ArtifactTree,
    ) {
        enum Op {
            Command(MaterializerCommand),
            RefreshTtls,
        }

        // Each Declare bumps the version, so that if an artifact is declared
        // a second time mid materialization of its previous version, we don't
        // incorrectly assume we materialized the latest version. We start with
        // 1 with because any disk state restored will start with version 0.
        let mut next_version = 1u64;

        let refresh_stream = if self.ttl_refresh_enabled {
            IntervalStream::new(tokio::time::interval_at(
                tokio::time::Instant::now() + self.ttl_refresh_frequency,
                self.ttl_refresh_frequency,
            ))
            .left_stream()
        } else {
            futures::stream::empty().right_stream()
        };

        let mut stream = futures::stream::select(
            UnboundedReceiverStream::new(commands).map(Op::Command),
            refresh_stream.map(|_instant| Op::RefreshTtls),
        );

        let mut current_ttl_refresh: Option<JoinHandle<()>> = None;

        while let Some(op) = stream.next().await {
            match op {
                Op::Command(command) => match command {
                    // Entry point for `get_materialized_file_paths` calls
                    MaterializerCommand::GetMaterializedFilePaths(paths, result_sender) => {
                        let result = paths.into_map(|p| tree.file_contents_path(p));
                        result_sender.send(result).ok();
                    }
                    MaterializerCommand::DeclareExisting(artifacts) => {
                        for (path, artifact) in artifacts {
                            self.declare_existing(&mut tree, path, artifact, next_version);
                            next_version += 1;
                        }
                    }
                    // Entry point for `declare_{copy|cas}` calls
                    MaterializerCommand::Declare(path, value, method) => {
                        self.declare(&mut tree, path, value, method, next_version);
                        next_version += 1;
                    }
                    MaterializerCommand::MatchArtifacts(paths, sender) => {
                        let all_matches = paths
                            .into_iter()
                            .all(|(path, value)| self.match_artifact(&mut tree, path, value));
                        sender.send(all_matches).ok();
                    }
                    MaterializerCommand::InvalidateFilePaths(paths, sender) => {
                        tracing::trace!(
                            paths = ?paths,
                            "invalidate paths",
                        );
                        let (invalidated_paths, existing_futs) =
                            tree.invalidate_paths_and_collect_futures(paths);
                        let sqlite_db = self.sqlite_db.dupe();
                        let invalidation_fut = tokio::task::spawn(async move {
                            join_all_existing_futs(existing_futs).await?;

                            // The existing futures can be in-flight materializations for
                            // the paths we are about to invalidate. The last thing those
                            // futures will do is insert into the SQLite DB, so we need to
                            // wait until they finish before we delete said rows.
                            // TODO(scottcao): add a unit test to test this behavior
                            if let Some(sqlite_db) = sqlite_db {
                                sqlite_db
                                    .materializer_state_table()
                                    .delete(invalidated_paths)
                                    .await?;
                            }

                            Ok(())
                        })
                        .map(|r| match r {
                            Ok(r) => r,
                            Err(e) => Err(e.into()), // Turn the JoinError into a SharedError.
                        })
                        .boxed()
                        .shared();
                        sender.send(invalidation_fut).ok();
                    }
                    // Entry point for `ensure_materialized` calls
                    MaterializerCommand::Ensure(paths, event_dispatcher, fut_sender) => {
                        fut_sender
                            .send(self.dupe().materialize_many_artifacts(
                                &mut tree,
                                paths,
                                event_dispatcher,
                            ))
                            .ok();
                    }
                    // Materialization of artifact succeeded
                    MaterializerCommand::MaterializationFinished {
                        path,
                        version,
                        result,
                    } => {
                        tree.materialization_finished(
                            path,
                            version,
                            result,
                            self.io_executor.dupe(),
                            // materialization_finished transitions the entry to Declared stage on errors,
                            // in which case the version of the newly declared artifact should be bumped.
                            // Let materialization_finished always consume a version in case the entry
                            // gets redeclared.
                            next_version,
                            self.sqlite_db.dupe(),
                        );
                        next_version += 1;
                    }
                    MaterializerCommand::Extension(ext) => {
                        ext.execute(&tree, &self.re_client_manager)
                    }
                },
                Op::RefreshTtls => {
                    // It'd be neat to just implement this in the refresh_stream itself and simply
                    // have this loop implicitly drive it, but we can't do that as the stream's
                    // and_then callback would have to capture `&tree`. So, instead, we store the
                    // JoinHandle and just avoid scheduling more than one, though this means we'll
                    // just miss ticks if we do take longer than a tick to run.

                    let curr = match current_ttl_refresh.take() {
                        Some(mut curr) => match futures::poll!(&mut curr) {
                            std::task::Poll::Ready(..) => None,
                            std::task::Poll::Pending => Some(curr),
                        },
                        None => None,
                    };

                    current_ttl_refresh = match curr {
                        Some(task) => Some(task),
                        None => create_ttl_refresh(
                            &tree,
                            &self.re_client_manager,
                            self.ttl_refresh_min_ttl,
                        )
                        .map(|fut| {
                            tokio::task::spawn(async move {
                                match fut.await {
                                    Ok(()) => {
                                        tracing::info!("Scheduled TTL refresh succeeded");
                                    }
                                    Err(e) => {
                                        tracing::warn!("Scheduled TTL refresh failed: {:#}", e);
                                    }
                                }
                            })
                        }),
                    };
                }
            }
        }
    }

    fn materialize_many_artifacts(
        self: Arc<Self>,
        tree: &mut ArtifactTree,
        paths: Vec<ProjectRelativePathBuf>,
        event_dispatcher: EventDispatcher,
    ) -> BoxStream<'static, Result<(), MaterializationError>> {
        let tasks = paths.into_iter().filter_map(|path| {
            self.dupe()
                .materialize_artifact(tree, path.as_ref(), event_dispatcher.dupe())
                .map(move |fut| {
                    fut.map_err(move |e| match e {
                        SharedMaterializingError::Error(source) => MaterializationError::Error {
                            path,
                            source: source.into(),
                        },
                        SharedMaterializingError::NotFound { info } => {
                            MaterializationError::NotFound { path, info }
                        }
                        SharedMaterializingError::SqliteDbError(source) => {
                            MaterializationError::SqliteDbError {
                                path,
                                source: source.into(),
                            }
                        }
                    })
                })
        });

        tasks.collect::<FuturesOrdered<_>>().boxed()
    }

    fn declare_existing<'a>(
        &self,
        tree: &'a mut ArtifactTree,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        version: u64,
    ) {
        tree.insert(
            path.iter().map(|f| f.to_owned()),
            box ArtifactMaterializationData {
                deps: value.deps().duped(),
                stage: ArtifactMaterializationStage::Materialized {
                    metadata: value.entry().dupe().into(),
                },
                version,
                processing_fut: None,
            },
        );
    }

    fn declare<'a>(
        &self,
        tree: &'a mut ArtifactTree,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        method: Box<ArtifactMaterializationMethod>,
        version: u64,
    ) {
        // Check if artifact to be declared is same as artifact that's already materialized.
        if let Some(data) = tree.prefix_get_mut(&mut path.iter()) {
            match &data.stage {
                ArtifactMaterializationStage::Materialized { metadata, .. } => {
                    // For checking if artifact is already materialized, we just
                    // need to check that the entry matches. If the deps are different
                    // we can just update them but keep the artifact as materialized.
                    let new_metadata: ArtifactMetadata = value.entry().dupe().into();
                    if metadata == &new_metadata {
                        // In this case, the entry declared matches the already materialized
                        // entry on disk, so just update the deps field but leave
                        // the artifact as materialized.
                        tracing::trace!(
                            path = %path,
                            "already materialized, updating deps only",
                        );
                        let deps = value.deps().duped();
                        data.stage = ArtifactMaterializationStage::Materialized {
                            metadata: metadata.dupe(),
                        };
                        data.deps = deps;

                        return;
                    }
                }
                _ => {}
            }
        }

        // We don't have a matching artifact. Declare it.
        tracing::trace!(
            path = %path,
            method = %method,
            value = %value.entry(),
            version = version,
            "declare artifact",
        );
        // Always invalidate materializer state before actual deleting from filesystem
        // so there will never be a moment where artifact is deleted but materializer
        // thinks it still exists.
        let (invalidated_paths, existing_futs) =
            tree.invalidate_paths_and_collect_futures(vec![path.clone()]);
        let data = box ArtifactMaterializationData {
            deps: value.deps().duped(),
            stage: ArtifactMaterializationStage::Declared {
                entry: value.entry().dupe(),
                method: Arc::new(*method),
            },
            version,
            processing_fut: Some(ProcessingFuture::Cleaning(clean_output_paths(
                self.io_executor.dupe(),
                path.clone(),
                // In order to make sure we don't have any race conditions when deleting,
                // we need to wait for all existing I/O futures to finish before
                // running out own cleaning future. In the case `invalidate_paths_and_collect_futures`
                // removed an entire sub-trie, we need to wait for all futures from that
                // sub-trie to finish first.
                Some(existing_futs),
                self.sqlite_db.dupe(),
                invalidated_paths,
            ))),
        };
        tree.insert(path.iter().map(|f| f.to_owned()), data);
    }

    /// Check if artifact to be declared is same as artifact that's already materialized.
    #[instrument(level = "debug", skip(self, tree), fields(path = %path, value = %value.entry()))]
    fn match_artifact(
        &self,
        tree: &mut ArtifactTree,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
    ) -> bool {
        let mut path_iter = path.iter();
        let data = match tree.prefix_get_mut(&mut path_iter) {
            Some(data) => data,
            None => {
                tracing::trace!("overlapping below");
                return false;
            }
        };

        // Something was declared above our path.
        if path_iter.next().is_some() {
            tracing::trace!("overlapping above");
            return false;
        }

        let is_match = match &data.stage {
            ArtifactMaterializationStage::Materialized { metadata, .. } => {
                let new_metadata: ArtifactMetadata = value.entry().dupe().into();
                let is_match = *metadata == new_metadata;
                tracing::trace!("materialized: found {}, is_match: {}", metadata.0, is_match);
                is_match
            }
            ArtifactMaterializationStage::Declared { entry, .. } => {
                // NOTE: In theory, if something was declared here, we should probably be able to
                // just re-declare over it?
                let is_match = value.entry() == entry;
                tracing::trace!("declared: found {}, is_match: {}", entry, is_match);
                is_match
            }
        };

        // In practice, having a matching artifact with different deps isn't actually *possible*
        // right now, because the deps are derived from the artifact value and we'll always have
        // declared them before. But, if we have a local action cache and persist that as well as
        // materializer state across restarts, then eventually we could have a match with something
        // that hasn't had its deps populated yet (sicne the materializer state does not know about
        // deps).
        if is_match {
            if let Some(deps) = value.deps() {
                data.deps = Some(deps.dupe())
            }
        }

        is_match
    }

    #[instrument(level = "debug", skip(self, tree), fields(path = %path))]
    fn materialize_artifact(
        self: Arc<Self>,
        tree: &mut ArtifactTree,
        mut path: &ProjectRelativePath,
        event_dispatcher: EventDispatcher,
    ) -> Option<MaterializingFuture> {
        // Get the data about the artifact, or return early if materializing/materialized
        let mut path_iter = path.iter();
        let data = match tree.prefix_get(&mut path_iter) {
            // Never declared, nothing to do
            None => {
                tracing::debug!("not known");
                return None;
            }
            Some(data) => data,
        };

        // Rewind the `path` up to the entry we *actually* found.
        for _ in path_iter {
            path = path
                .parent()
                .expect("Path iterator cannot cause us to rewind past the last parent");
        }

        let cleaning_fut = match &data.processing_fut {
            Some(ProcessingFuture::Cleaning(f)) => Some(f.clone()),
            Some(ProcessingFuture::Materializing(f)) => {
                tracing::debug!("join existing future");
                return Some(f.clone());
            }
            None => None,
        };

        let deps = data.deps.dupe();
        let check_deps = deps.is_some();
        let entry_and_method = match &data.stage {
            ArtifactMaterializationStage::Declared { entry, method } => {
                Some((entry.dupe(), method.dupe()))
            }
            ArtifactMaterializationStage::Materialized {
                metadata: _metadata,
            } => match check_deps {
                true => None,
                false => {
                    tracing::debug!(
                        path = %path,
                        "already materialized, nothing to do"
                    );
                    return None;
                }
            },
        };
        let version = data.version;

        tracing::debug!(
            has_entry_and_method = entry_and_method.is_some(),
            method = ?entry_and_method.as_ref().map(|(_, m)| m),
            has_deps = deps.is_some(),
            version = version,
            cleaning = cleaning_fut.is_some(),
            "materialize artifact"
        );

        // If the artifact copies from other artifacts, we must materialize them first
        let deps_tasks = match entry_and_method.as_ref() {
            Some((_, m)) => match m.as_ref() {
                ArtifactMaterializationMethod::CasDownload { .. }
                | ArtifactMaterializationMethod::HttpDownload { .. }
                | ArtifactMaterializationMethod::Write { .. } => Vec::new(),
                ArtifactMaterializationMethod::LocalCopy(_, copied_artifacts) => copied_artifacts
                    .iter()
                    .filter_map(|a| {
                        self.dupe().materialize_artifact(
                            tree,
                            a.src.as_ref(),
                            event_dispatcher.dupe(),
                        )
                    })
                    .collect::<Vec<_>>(),
            },
            _ => Vec::new(),
        };

        // The artifact might have symlinks pointing to other artifacts. We must
        // materialize them as well, to avoid dangling synlinks.
        let link_deps_tasks = match deps.as_ref() {
            None => Vec::new(),
            Some(deps) => tree
                .find_artifacts(deps)
                .into_iter()
                .filter_map(|p| {
                    self.dupe()
                        .materialize_artifact(tree, p.as_ref(), event_dispatcher.dupe())
                })
                .collect::<Vec<_>>(),
        };

        // Create a task to await deps and materialize ourselves
        let path_buf = path.to_buf();
        let path_buf_dup = path_buf.clone();
        let command_sender = self.command_sender.clone();
        let sqlite_db = self.sqlite_db.dupe();
        let task = tokio::task::spawn(async move {
            // Materialize the deps and this entry. This *must* happen in a try block because we
            // need to notity the materializer regardless of whether this succeeds or fails.

            let res: Result<(), SharedMaterializingError> = try {
                // If there is an existing future trying to delete conflicting paths, we must wait for it
                // to finish before we can start materialization.
                if let Some(cleaning_fut) = cleaning_fut {
                    cleaning_fut
                        .await
                        .with_context(|| format!(
                            "Error waiting for a previous future to finish cleaning output path {}",
                            &path_buf
                        ))
                        .map_err(|e| SharedMaterializingError::Error(e.into()))?;
                };

                // In case this is a local copy, we first need to materialize the
                // artifacts we are copying from, before we can copy them.
                for t in deps_tasks {
                    t.await?;
                }

                if let Some((entry, method)) = entry_and_method {
                    // All potential deps are materialized. Now materialize the entry.
                    self.dupe()
                        .materialize_entry(
                            path_buf.clone(),
                            method,
                            entry.dupe(),
                            event_dispatcher.dupe(),
                        )
                        .await?;

                    // Record in sqlite that this artifact is now materialized
                    if let Some(sqlite_db) = sqlite_db {
                        sqlite_db
                            .materializer_state_table()
                            .insert(path_buf.clone(), entry.into())
                            .await
                            .shared_error()
                            .map_err(SharedMaterializingError::SqliteDbError)?;
                    }
                };

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
            });

            res
        })
        .map(|r| match r {
            Ok(r) => r,
            Err(e) => Err(SharedMaterializingError::Error(e.into())), // Turn the JoinError into a SharedError.
        })
        .boxed()
        .shared();

        let data = tree.prefix_get_mut(&mut path.iter()).unwrap();
        data.processing_fut = Some(ProcessingFuture::Materializing(task.clone()));

        Some(task)
    }

    /// Materializes an `entry` at `path`, using the materialization `method`
    #[instrument(level = "debug", skip(self, stat), fields(path = %path, method = %method, entry = %entry))]
    async fn materialize_entry_span(
        self: Arc<Self>,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        stat: &mut MaterializationStat,
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
                            let digest = maybe_tombstone_digest(f.digest.data())?.to_re();

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
                stat.file_count = files.len().try_into().unwrap_or_default();
                stat.total_bytes = files
                    .iter()
                    .map(|x| u64::try_from(x.named_digest.digest.size_in_bytes).unwrap_or_default())
                    .sum();

                let connection = self.re_client_manager.get_re_connection();
                let re_client = connection.get_client();

                re_client
                    .materialize_files(files, info.re_use_case)
                    .await
                    .map_err(|e| match e.downcast_ref::<REClientError>() {
                        Some(e) if e.code == TCode::NOT_FOUND => {
                            MaterializeEntryError::NotFound { info: info.dupe() }
                        }
                        _ => MaterializeEntryError::Error(e.context({
                            format!("Error materializing files declared by action: {}", info)
                        })),
                    })?;
            }
            ArtifactMaterializationMethod::HttpDownload { info } => {
                async {
                    let downloaded = http_download(
                        &http_client()?,
                        &self.fs,
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
                    if downloaded.size() != info.metadata.digest.size() {
                        return Err(anyhow::anyhow!(
                            "Downloaded size ({}) does not match expected size ({})",
                            downloaded.size(),
                            info.metadata.digest.size(),
                        ));
                    }
                    stat.file_count = 1;
                    stat.total_bytes = info.metadata.digest.size();
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
                    .execute_io_inline(|| {
                        for a in copied_artifacts {
                            let count_and_bytes = a.dest_entry.calc_output_count_and_bytes();
                            stat.file_count += count_and_bytes.count;
                            stat.total_bytes += count_and_bytes.bytes;

                            materialize_files(
                                a.dest_entry.as_ref(),
                                &self.fs.root().join(&a.src),
                                &self.fs.root().join(&a.dest),
                            )?;
                        }
                        Ok(())
                    })
                    .await?;
            }
            ArtifactMaterializationMethod::Write {
                compressed_data,
                decompressed_size,
                is_executable,
                ..
            } => {
                stat.file_count = 1;
                self.io_executor
                    .execute_io_inline(|| {
                        let data = zstd::bulk::decompress(compressed_data, *decompressed_size)
                            .context("Error decompressing data")?;
                        stat.total_bytes = *decompressed_size as u64;
                        self.fs.write_file(&path, &data, *is_executable)
                    })
                    .await?;
            }
        };
        Ok(())
    }

    /// Materializes an `entry` at `path`, using the materialization `method`
    #[instrument(level = "debug", skip(self), fields(path = %path, method = %method, entry = %entry))]
    async fn materialize_entry(
        self: Arc<Self>,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        event_dispatcher: EventDispatcher,
    ) -> Result<(), MaterializeEntryError> {
        let materialization_start = buck2_data::MaterializationStart {
            action_digest: match method.as_ref() {
                ArtifactMaterializationMethod::CasDownload { info } => {
                    info.action_digest().map(|digest| digest.to_string())
                }
                _ => None,
            },
        };
        event_dispatcher
            .span_async(materialization_start, async {
                let path_string = path.as_str().to_owned();
                let mut stat = MaterializationStat {
                    file_count: 0,
                    total_bytes: 0,
                };
                let res = self
                    .materialize_entry_span(path, method.dupe(), entry, &mut stat)
                    .await;
                let error = res.as_ref().err().map(|e| format!("{:#}", e));

                (
                    res,
                    buck2_data::MaterializationEnd {
                        action_digest: None,
                        file_count: stat.file_count,
                        total_bytes: stat.total_bytes,
                        path: path_string,
                        success: error.is_none(),
                        error,
                        method: Some(method.to_proto() as i32),
                    },
                )
            })
            .await?;
        Ok(())
    }
}

impl ArtifactTree {
    /// Given a path that's (possibly) not yet materialized, returns the path
    /// `contents_path` where its contents can be found. Returns Err if the
    /// contents cannot be found (ex. if it requires HTTP or CAS download)
    ///
    /// Note that the returned `contents_path` could be the same as `path`.
    #[instrument(level = "trace", skip(self), fields(path = %path))]
    fn file_contents_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason> {
        let mut path_iter = path.iter();
        let materialization_data = match self.prefix_get(&mut path_iter) {
            // Not in tree. Assume it's a source file that doesn't require materialization from materializer.
            None => return Ok(path),
            Some(data) => data,
        };
        let (entry, method) = match &materialization_data.stage {
            ArtifactMaterializationStage::Materialized { .. } => {
                return Ok(path);
            }
            ArtifactMaterializationStage::Declared { entry, method } => {
                (entry.dupe(), method.dupe())
            }
        };
        match method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { info } => {
                let path_iter = path_iter.peekable();

                let root_entry = entry.dupe();
                let mut entry = Some(entry.as_ref().map_dir(|d| d as &dyn ActionDirectory));

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
                            entry: root_entry,
                            info: info.dupe(),
                        },
                    ),
                }
            }
            ArtifactMaterializationMethod::HttpDownload { .. }
            | ArtifactMaterializationMethod::Write { .. } => {
                // TODO: Do the write directly to RE instead of materializing locally?
                Err(ArtifactNotMaterializedReason::RequiresMaterialization { path })
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

    #[instrument(level = "debug", skip(self, result, io_executor, sqlite_db), fields(path = %artifact_path, version = %version))]
    fn materialization_finished(
        &mut self,
        artifact_path: ProjectRelativePathBuf,
        version: u64,
        result: Result<(), SharedMaterializingError>,
        io_executor: Arc<dyn BlockingExecutor>,
        next_version: u64,
        sqlite_db: Option<Arc<MaterializerStateSqliteDb>>,
    ) {
        match self.prefix_get_mut(&mut artifact_path.iter()) {
            Some(mut info) => {
                if info.version > version {
                    tracing::debug!("version conflict");
                    return;
                }

                // We can only unset the future if version matches.
                // Otherwise, we may be unsetting a different future from a newer version.
                info.processing_fut = None;

                if result.is_err() {
                    tracing::debug!("materialization failed, redeclaring artifact");
                    // Bump the version here because the artifact is redeclared.
                    info.version = next_version;
                    // Even though materialization failed, something may have still materialized at artifact_path,
                    // so we need to delete anything at artifact_path before we ever retry materializing it.
                    // TODO(scottcao): Once command processor accepts an ArtifactTree instead of initializing one,
                    // add a test case to ensure this behavior.
                    info.processing_fut = Some(ProcessingFuture::Cleaning(clean_output_paths(
                        io_executor,
                        artifact_path.clone(),
                        None,
                        // It may be possible that sqlite insertion failed but still inserted
                        // an entry in the db, in which case we should delete it.
                        sqlite_db,
                        vec![artifact_path],
                    )));
                } else {
                    tracing::debug!(has_deps = info.deps.is_some(), "transition to Materialized");
                    let entry = match &info.stage {
                        ArtifactMaterializationStage::Materialized { .. } => {
                            tracing::debug!("artifact is somehow already marked materialized");
                            return;
                        }
                        ArtifactMaterializationStage::Declared {
                            entry,
                            method: _method,
                        } => entry.dupe(),
                    };
                    let metadata = entry.into();
                    info.stage = ArtifactMaterializationStage::Materialized { metadata };
                }
            }
            None => {
                tracing::debug!("materialization_finished but path is vacant!")
            }
        }
    }

    /// Removes paths from tree and returns a pair of two vecs.
    /// First vec is a list of paths removed. Second vec is a list of
    /// pairs of removed paths to futures that haven't finished.
    fn invalidate_paths_and_collect_futures(
        &mut self,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> (
        Vec<ProjectRelativePathBuf>,
        Vec<(ProjectRelativePathBuf, ProcessingFuture)>,
    ) {
        let mut invalidated_paths = Vec::new();
        let mut futs = Vec::new();
        for path in paths {
            for (path, data) in self.remove_path(&path) {
                if let Some(processing_fut) = data.processing_fut {
                    futs.push((path.clone(), processing_fut));
                }
                invalidated_paths.push(path);
            }
        }
        (invalidated_paths, futs)
    }
}

impl<V: 'static> FileTree<V> {
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

    /// Removes path from FileTree. Returns an iterator of pairs of path and entry removed
    /// from the tree.
    fn remove_path(
        &mut self,
        path: &ProjectRelativePath,
    ) -> Box<dyn Iterator<Item = (ProjectRelativePathBuf, V)>> {
        let mut path_iter = path.iter();
        let removed = self.remove(&mut path_iter);

        let mut path = path;
        // Rewind the `path` up to the entry we *actually* found.
        for _ in path_iter {
            path = path
                .parent()
                .expect("Path iterator cannot cause us to rewind past the last parent");
        }
        let path = path.to_owned();

        match removed {
            Some(tree) => box tree
                .into_iter()
                .with_paths()
                .map(move |(k, v)| ((path).join(k), v)),
            None => box std::iter::empty(),
        }
    }
}

trait WithPathsIterator<K, V>: Iterator<Item = (VecDeque<K>, V)>
where
    K: AsRef<FileName>,
{
    fn with_paths<'a>(self) -> Box<dyn Iterator<Item = (ProjectRelativePathBuf, V)> + 'a>
    where
        Self: Sized + 'a,
    {
        box self
            .into_iter()
            .map(|(k, v)| -> (ProjectRelativePathBuf, V) {
                let path = k
                    .iter()
                    .map(|f| f.as_ref())
                    .collect::<Option<ForwardRelativePathBuf>>()
                    .unwrap_or_else(|| ForwardRelativePathBuf::unchecked_new("".to_owned()));
                (path.into(), v)
            })
    }
}

pub type FileTreeIterator<'a, V> = DataTreeIterator<'a, FileNameBuf, V>;
pub type FileTreeIntoIterator<V> = DataTreeIntoIterator<FileNameBuf, V>;

impl<'a, V: 'static> WithPathsIterator<&'a FileNameBuf, &'a V> for FileTreeIterator<'a, V> {}

impl<V: 'static> WithPathsIterator<FileNameBuf, V> for FileTreeIntoIterator<V> {}

/// This is used for testing to ingest digests (via BUCK2_TEST_TOMBSTONED_DIGESTS).
fn maybe_tombstone_digest(digest: &FileDigest) -> anyhow::Result<&FileDigest> {
    // This has to be of size 1 since size 0 will result in the RE client just producing an empty
    // instead of a not-found error.
    static TOMBSTONE_DIGEST: Lazy<FileDigest> = Lazy::new(|| FileDigest::new_sha1([0; 20], 1));

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

    if let Some(digests) = TOMBSTONED_DIGESTS.get()? {
        if digests.contains(digest) {
            return Ok(&*TOMBSTONE_DIGEST);
        }
    }

    Ok(digest)
}

/// Wait on all futures in `futs` to finish. Return Error for first future that failed
/// in the Vec.
async fn join_all_existing_futs(
    existing_futs: Vec<(ProjectRelativePathBuf, ProcessingFuture)>,
) -> SharedResult<()> {
    // We can await inside a loop here because all ProcessingFuture's are spawned.
    for (path, fut) in existing_futs.into_iter() {
        match fut {
            ProcessingFuture::Materializing(f) => {
                // We don't care about errors from previous materializations.
                // We are trying to delete anything that has been materialized,
                // so these errors can be ignored.
                f.await.ok();
            }
            ProcessingFuture::Cleaning(f) => {
                f.await.with_context(|| {
                    format!(
                        "Error waiting for a previous future to finish cleaning output path {}",
                        path
                    )
                })?;
            }
        };
    }

    Ok(())
}

/// Spawns a future to clean output paths while waiting for any
/// pending future to finish.
fn clean_output_paths(
    io_executor: Arc<dyn BlockingExecutor>,
    path: ProjectRelativePathBuf,
    existing_futs: Option<Vec<(ProjectRelativePathBuf, ProcessingFuture)>>,
    sqlite_db: Option<Arc<MaterializerStateSqliteDb>>,
    invalidated_paths: Vec<ProjectRelativePathBuf>,
) -> CleaningFuture {
    tokio::task::spawn(async move {
        if let Some(existing_futs) = existing_futs {
            join_all_existing_futs(existing_futs).await?;
        }

        // See comment block below the previous call site of `join_all_existing_futs`
        // in the `MaterializerCommand::InvalidateFilePaths` arm of the match statement
        // to see why we need to do the sqlite deletion after all existing futures are
        // finished.
        if let Some(sqlite_db) = sqlite_db {
            sqlite_db
                .materializer_state_table()
                .delete(invalidated_paths)
                .await?;
        }

        io_executor
            .dupe()
            .execute_io(box CleanOutputPaths { paths: vec![path] })
            .await
            .shared_error()
    })
    .map(|r| match r {
        Ok(r) => r,
        Err(e) => Err(e.into()), // Turn the JoinError into a SharedError.
    })
    .boxed()
    .shared()
}

/// Spawn a task to refresh TTLs.
fn create_ttl_refresh(
    tree: &ArtifactTree,
    re_manager: &Arc<ReConnectionManager>,
    min_ttl: Duration,
) -> Option<impl Future<Output = anyhow::Result<()>>> {
    let mut digests_to_refresh = HashSet::new();

    let ttl_deadline = Utc::now() + min_ttl;

    for (_, data) in tree.iter() {
        match &data.stage {
            ArtifactMaterializationStage::Declared { method, .. } => match method.as_ref() {
                ArtifactMaterializationMethod::CasDownload { info } => {
                    if let Some(action_digest) = info.action_digest() {
                        if action_digest.expires() <= ttl_deadline {
                            digests_to_refresh.insert((action_digest.dupe(), info.re_use_case));
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    if digests_to_refresh.is_empty() {
        None
    } else {
        let re_manager = re_manager.dupe();

        Some(async move {
            let re_connection = re_manager.get_re_connection();
            let re_client = re_connection.get_client();
            let re_client = &re_client;

            futures::future::join_all(digests_to_refresh.iter().map(
                |(digest, use_case)| async move {
                    // A side effect of action cache queries is to refresh the underlying outputs.
                    match re_client
                        .action_cache(digest.data().dupe(), *use_case)
                        .await
                    {
                        Ok(Some(res)) => {
                            let expires = Utc::now() + Duration::seconds(res.ttl);
                            digest.update_expires(expires);
                            tracing::debug!("Updated expiry for action `{}`: {}", digest, expires)
                        }
                        Ok(None) => {
                            tracing::info!(
                                "Action `{}` is referenced by materializer, but expired",
                                digest
                            );
                        }
                        Err(e) => {
                            tracing::info!(
                                "Failed to query action cache for action `{}`: {:#}",
                                digest,
                                e
                            );
                        }
                    }
                },
            ))
            .await;

            // Currently we don't propagate errors back here.
            Ok(())
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_common::file_ops::FileMetadata;
    use buck2_execute::directory::insert_file;
    use buck2_execute::directory::ActionDirectoryBuilder;
    use gazebo::dupe::Dupe;

    use super::*;

    #[test]
    fn test_find_artifacts() -> anyhow::Result<()> {
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

    #[test]
    fn test_remove_path() {
        fn insert(tree: &mut FileTree<String>, path: &str) {
            tree.insert(
                ProjectRelativePath::unchecked_new(path)
                    .iter()
                    .map(|f| f.to_owned()),
                path.to_owned(),
            );
        }

        let mut tree: FileTree<String> = FileTree::new();
        insert(&mut tree, "a/b/c/d");
        insert(&mut tree, "a/b/c/e");
        insert(&mut tree, "a/c");

        let removed_subtree = tree.remove_path(ProjectRelativePath::unchecked_new("a/b"));
        // Convert to HashMap<String, String> so it's easier to test
        let removed_subtree: HashMap<String, String> = removed_subtree
            .map(|(k, v)| (k.as_str().to_owned(), v))
            .collect();

        assert_eq!(removed_subtree.len(), 2);
        assert_eq!(removed_subtree.get("a/b/c/d"), Some(&"a/b/c/d".to_owned()));
        assert_eq!(removed_subtree.get("a/b/c/e"), Some(&"a/b/c/e".to_owned()));
    }
}
