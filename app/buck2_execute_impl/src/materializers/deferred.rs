/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub mod clean_stale;
mod data_tree;
mod extension;
mod io_handler;
mod materialize_stack;
mod subscriptions;

pub(crate) mod artifact_tree;
mod command_processor;
pub mod directory_metadata;
pub(crate) mod file_tree;
#[cfg(test)]
mod tests;

use std::collections::HashSet;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use artifact_tree::ArtifactMaterializationMethod;
use artifact_tree::ArtifactMaterializationStage;
use artifact_tree::Processing;
use artifact_tree::ProcessingFuture;
use async_trait::async_trait;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_common::liveliness_observer::LivelinessGuard;
use buck2_core::buck2_env;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_iterator::DirectoryIteratorPathStack;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::dispatch::current_span;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::get_dispatcher_opt;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::materialize::materializer::ArtifactNotMaterializedReason;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::CasNotFoundError;
use buck2_execute::materialize::materializer::CopiedArtifact;
use buck2_execute::materialize::materializer::DeclareArtifactPayload;
use buck2_execute::materialize::materializer::DeclareMatchOutcome;
use buck2_execute::materialize::materializer::DeferredMaterializerExtensions;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use buck2_execute::materialize::materializer::MaterializationError;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_execute::re::manager::ReConnectionManager;
use buck2_http::HttpClient;
use buck2_util::threads::thread_spawn;
use chrono::DateTime;
use chrono::Duration;
use chrono::Utc;
use derivative::Derivative;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::stream::BoxStream;
use parking_lot::RwLock;
use tokio::runtime::Handle;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

use crate::materializers::deferred::artifact_tree::ArtifactTree;
use crate::materializers::deferred::artifact_tree::Version;
use crate::materializers::deferred::clean_stale::CleanStaleConfig;
use crate::materializers::deferred::command_processor::DeferredMaterializerCommandProcessor;
use crate::materializers::deferred::command_processor::LowPriorityMaterializerCommand;
use crate::materializers::deferred::command_processor::MaterializerCommand;
use crate::materializers::deferred::file_tree::FileTree;
use crate::materializers::deferred::io_handler::DefaultIoHandler;
use crate::materializers::deferred::io_handler::IoHandler;
use crate::sqlite::materializer_db::MaterializerState;
use crate::sqlite::materializer_db::MaterializerStateSqliteDb;

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
pub struct DeferredMaterializerAccessor<T: IoHandler + 'static> {
    /// Sender to emit commands to the command loop. See `MaterializerCommand`.
    #[allocative(skip)]
    command_sender: Arc<MaterializerSender<T>>,
    /// Handle of the command loop thread. Aborted on Drop.
    /// This thread serves as a queue for declare/ensure requests, making
    /// sure only one executes at a time and in the order they came in.
    /// TODO(rafaelc): aim to replace it with a simple mutex.
    #[allocative(skip)]
    #[cfg_attr(not(test), expect(dead_code))]
    command_thread: Option<std::thread::JoinHandle<()>>,
    /// Determines what to do on `try_materialize_final_artifact`: if true,
    /// materializes them, otherwise skips them.
    materialize_final_artifacts: bool,
    defer_write_actions: bool,

    io: Arc<T>,

    /// Tracked for logging purposes.
    materializer_state_info: buck2_data::MaterializerStateInfo,

    stats: Arc<DeferredMaterializerStats>,
}

pub type DeferredMaterializer = DeferredMaterializerAccessor<DefaultIoHandler>;

impl<T: IoHandler> Drop for DeferredMaterializerAccessor<T> {
    fn drop(&mut self) {
        // We don't try to stop the underlying thread, since in practice when we drop the
        // DeferredMaterializer we are about to just terminate the process.
    }
}

/// Statistics we collect while operating the Deferred Materializer.
#[derive(Allocative, Default)]
pub struct DeferredMaterializerStats {
    declares: AtomicU64,
    declares_reused: AtomicU64,
}

fn access_time_update_max_buffer_size() -> buck2_error::Result<usize> {
    buck2_env!("BUCK_ACCESS_TIME_UPDATE_MAX_BUFFER_SIZE", type=usize, default=5000)
}

pub struct DeferredMaterializerConfigs {
    pub materialize_final_artifacts: bool,
    pub defer_write_actions: bool,
    pub ttl_refresh: TtlRefreshConfiguration,
    pub update_access_times: AccessTimesUpdates,
    pub verbose_materializer_log: bool,
    pub clean_stale_config: Option<CleanStaleConfig>,
    pub disable_eager_write_dispatch: bool,
}

pub struct TtlRefreshConfiguration {
    pub frequency: std::time::Duration,
    pub min_ttl: Duration,
    pub enabled: bool,
}

#[derive(Clone, Copy, Debug, Dupe, PartialEq)]
pub enum AccessTimesUpdates {
    /// Flushes when the buffer is full and periodically
    Full,
    ///Flushes only when buffer is full
    Partial,
    /// Does not flush at all
    Disabled,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub enum AccessTimesUpdatesError {
    #[error(
        "Invalid value for buckconfig `[buck2] update_access_times`. Got `{0}`. Expected one of `full`, `partial`  or `disabled`."
    )]
    InvalidValueForConfig(String),
}

impl AccessTimesUpdates {
    pub fn try_new_from_config_value(config_value: Option<&str>) -> buck2_error::Result<Self> {
        match config_value {
            None | Some("") | Some("full") => Ok(AccessTimesUpdates::Full),
            Some("partial") => Ok(AccessTimesUpdates::Partial),
            Some("disabled") => Ok(AccessTimesUpdates::Disabled),
            Some(v) => Err(AccessTimesUpdatesError::InvalidValueForConfig(v.to_owned()).into()),
        }
    }
}

#[derive(Copy, Dupe, Clone)]
struct MaterializerCounters {
    sent: &'static AtomicUsize,
    received: &'static AtomicUsize,
}

impl MaterializerCounters {
    /// New counters. Note that this leaks the underlying data. See comments on MaterializerSender.
    fn leak_new() -> Self {
        Self {
            sent: Box::leak(Box::new(AtomicUsize::new(0))),
            received: Box::leak(Box::new(AtomicUsize::new(0))),
        }
    }

    fn ack_received(&self) {
        self.received.fetch_add(1, Ordering::Relaxed);
    }

    fn queue_size(&self) -> usize {
        self.sent
            .load(Ordering::Relaxed)
            .saturating_sub(self.received.load(Ordering::Relaxed))
    }
}

pub struct MaterializerSender<T: 'static> {
    /// High priority commands are processed in order.
    high_priority: mpsc::UnboundedSender<MaterializerCommand<T>>,
    /// Low priority commands are processed in order relative to each other, but high priority
    /// commands can be reordered ahead of them.
    low_priority: mpsc::UnboundedSender<LowPriorityMaterializerCommand>,
    counters: MaterializerCounters,
    /// Liveliness guard held while clean stale executes, dropped to interrupt clean.
    clean_guard: RwLock<Option<LivelinessGuard>>,
}

impl<T> MaterializerSender<T> {
    fn send(
        &self,
        command: MaterializerCommand<T>,
    ) -> Result<(), mpsc::error::SendError<MaterializerCommand<T>>> {
        {
            let read = self.clean_guard.read();
            if read.is_some() {
                drop(read);
                *self.clean_guard.write() = None;
            }
        }
        let res = self.high_priority.send(command);
        self.counters.sent.fetch_add(1, Ordering::Relaxed);
        res
    }

    fn send_low_priority(
        &self,
        command: LowPriorityMaterializerCommand,
    ) -> Result<(), mpsc::error::SendError<LowPriorityMaterializerCommand>> {
        let res = self.low_priority.send(command);
        self.counters.sent.fetch_add(1, Ordering::Relaxed);
        res
    }
}

struct MaterializerReceiver<T: 'static> {
    high_priority: mpsc::UnboundedReceiver<MaterializerCommand<T>>,
    low_priority: mpsc::UnboundedReceiver<LowPriorityMaterializerCommand>,
    counters: MaterializerCounters,
}

struct TtlRefreshHistoryEntry {
    at: DateTime<Utc>,
    outcome: Option<buck2_error::Result<()>>,
}

// NOTE: This doesn't derive `Error` and that's on purpose.  We don't want to make it easy (or
// possible, in fact) to add  `context` to this SharedProcessingError and lose the variant.
#[derive(Debug, Clone, Dupe)]
pub enum SharedMaterializingError {
    Error(buck2_error::Error),
    NotFound(CasNotFoundError),
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
pub enum MaterializeEntryError {
    #[error(transparent)]
    Error(buck2_error::Error),

    /// The artifact wasn't found. This typically means it expired in the CAS.
    #[error(transparent)]
    NotFound(CasNotFoundError),
}

impl From<buck2_error::Error> for MaterializeEntryError {
    fn from(e: buck2_error::Error) -> MaterializeEntryError {
        Self::Error(e)
    }
}

impl From<MaterializeEntryError> for SharedMaterializingError {
    fn from(e: MaterializeEntryError) -> SharedMaterializingError {
        match e {
            MaterializeEntryError::Error(e) => Self::Error(e.into()),
            MaterializeEntryError::NotFound(e) => Self::NotFound(e),
        }
    }
}

#[async_trait]
impl<T: IoHandler + Allocative> Materializer for DeferredMaterializerAccessor<T> {
    fn name(&self) -> &str {
        "deferred"
    }

    async fn declare_existing(
        &self,
        artifacts: Vec<DeclareArtifactPayload>,
    ) -> buck2_error::Result<()> {
        let cmd = MaterializerCommand::DeclareExisting(
            artifacts,
            current_span(),
            get_dispatcher_opt().map(|d| d.trace_id().dupe()),
        );
        self.command_sender.send(cmd)?;
        Ok(())
    }

    async fn declare_copy_impl(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> buck2_error::Result<()> {
        // TODO(rafaelc): get rid of this tree; it'd save a lot of memory.
        let mut srcs_tree = FileTree::new();
        for copied_artifact in srcs.iter() {
            let dest = copied_artifact.dest.strip_prefix(&path)?;

            {
                let mut walk = unordered_entry_walk(
                    copied_artifact
                        .dest_entry
                        .as_ref()
                        .map_dir(Directory::as_ref),
                );
                while let Some((path, entry)) = walk.next() {
                    if let DirectoryEntry::Leaf(ActionDirectoryMember::File(..)) = entry {
                        let path = path.get();
                        let dest_iter = dest.iter().chain(path.iter()).map(|f| f.to_owned());
                        let src = copied_artifact.src.join(&path);
                        srcs_tree.insert(dest_iter, src);
                    }
                }
            }
        }
        let cmd = MaterializerCommand::Declare(
            DeclareArtifactPayload {
                path,
                artifact: value,
                persist_full_directory_structure: false,
            },
            Box::new(ArtifactMaterializationMethod::LocalCopy(srcs_tree, srcs)),
            get_dispatcher(),
        );
        self.command_sender.send(cmd)?;
        Ok(())
    }

    async fn declare_cas_many_impl<'a, 'b>(
        &self,
        info: Arc<CasDownloadInfo>,
        artifacts: Vec<DeclareArtifactPayload>,
    ) -> buck2_error::Result<()> {
        for a in artifacts {
            let cmd = MaterializerCommand::Declare(
                a,
                Box::new(ArtifactMaterializationMethod::CasDownload { info: info.dupe() }),
                get_dispatcher(),
            );
            self.command_sender.send(cmd)?;
        }
        Ok(())
    }

    async fn declare_http(
        &self,
        path: ProjectRelativePathBuf,
        info: HttpDownloadInfo,
    ) -> buck2_error::Result<()> {
        let cmd = MaterializerCommand::Declare(
            DeclareArtifactPayload {
                path,
                artifact: ArtifactValue::file(info.metadata.dupe()),
                persist_full_directory_structure: false,
            },
            Box::new(ArtifactMaterializationMethod::HttpDownload { info }),
            get_dispatcher(),
        );
        self.command_sender.send(cmd)?;

        Ok(())
    }

    async fn declare_write<'a>(
        &self,
        generate: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> buck2_error::Result<Vec<ArtifactValue>> {
        if !self.defer_write_actions {
            return self.io.immediate_write(generate).await;
        }

        let contents = generate()?;

        let mut paths = Vec::with_capacity(contents.len());
        let mut values = Vec::with_capacity(contents.len());
        let mut methods = Vec::with_capacity(contents.len());

        for WriteRequest {
            path,
            content,
            is_executable,
        } in contents
        {
            let digest = TrackedFileDigest::from_content(
                &content,
                self.io.digest_config().cas_digest_config(),
            );

            let meta = FileMetadata {
                digest,
                is_executable,
            };

            // NOTE: The zstd crate doesn't release extra capacity of its encoding buffer so it's
            // important to do so here (or the compressed Vec is the same capacity as the input!).
            let compressed_data = zstd::bulk::compress(&content, 0)
                .with_buck_error_context(|| format!("Error compressing {} bytes", content.len()))?
                .into_boxed_slice();

            paths.push(path);
            values.push(ArtifactValue::file(meta));
            methods.push(ArtifactMaterializationMethod::Write(Arc::new(WriteFile {
                compressed_data,
                decompressed_size: content.len(),
                is_executable,
            })));
        }

        for (path, (value, method)) in std::iter::zip(paths, std::iter::zip(values.iter(), methods))
        {
            self.command_sender.send(MaterializerCommand::Declare(
                DeclareArtifactPayload {
                    path,
                    artifact: value.dupe(),
                    persist_full_directory_structure: false,
                },
                Box::new(method),
                get_dispatcher(),
            ))?;
        }

        Ok(values)
    }

    async fn declare_match(
        &self,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> buck2_error::Result<DeclareMatchOutcome> {
        let (sender, recv) = oneshot::channel();

        self.command_sender
            .send(MaterializerCommand::MatchArtifacts(artifacts, sender))?;

        let is_match = recv
            .await
            .buck_error_context("Recv'ing match future from command thread.")?;

        Ok(is_match.into())
    }

    async fn has_artifact_at(&self, path: ProjectRelativePathBuf) -> buck2_error::Result<bool> {
        let (sender, recv) = oneshot::channel();

        self.command_sender
            .send(MaterializerCommand::HasArtifact(path, sender))?;

        let has_artifact = recv
            .await
            .buck_error_context("Receiving \"has artifact\" future from command thread.")?;

        Ok(has_artifact)
    }

    async fn invalidate_many(&self, paths: Vec<ProjectRelativePathBuf>) -> buck2_error::Result<()> {
        let (sender, recv) = oneshot::channel();

        self.command_sender
            .send(MaterializerCommand::InvalidateFilePaths(
                paths,
                sender,
                get_dispatcher(),
            ))?;

        // Wait on future to finish before invalidation can continue.
        let invalidate_fut = recv.await?;
        invalidate_fut.await.map_err(buck2_error::Error::from)
    }

    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<BoxStream<'static, Result<(), MaterializationError>>> {
        let event_dispatcher = get_dispatcher();

        // TODO: display [materializing] in superconsole
        let (sender, recv) = oneshot::channel();
        self.command_sender
            .send(MaterializerCommand::Ensure(
                artifact_paths,
                event_dispatcher,
                sender,
            ))
            .buck_error_context("Sending Ensure() command.")?;
        let materialization_fut = recv
            .await
            .buck_error_context("Receiving materialization future from command thread.")?;
        Ok(materialization_fut)
    }

    async fn try_materialize_final_artifact(
        &self,
        artifact_path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<bool> {
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
    ) -> buck2_error::Result<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>>
    {
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

    fn log_materializer_state(&self, events: &EventDispatcher) {
        events.instant_event(self.materializer_state_info)
    }

    fn add_snapshot_stats(&self, snapshot: &mut buck2_data::Snapshot) {
        snapshot.deferred_materializer_declares = self.stats.declares.load(Ordering::Relaxed);
        snapshot.deferred_materializer_declares_reused =
            self.stats.declares_reused.load(Ordering::Relaxed);
        snapshot.deferred_materializer_queue_size = self.command_sender.counters.queue_size() as _;
    }

    async fn get_artifact_entries_for_materialized_paths(
        &self,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<
        Vec<
            Option<(
                ProjectRelativePathBuf,
                ActionDirectoryEntry<ActionSharedDirectory>,
            )>,
        >,
    > {
        let (sender, recv) = oneshot::channel();

        self.command_sender
            .send(MaterializerCommand::GetArtifactEntriesForMaterializedPaths(
                paths, sender,
            ))?;

        let result = recv.await.buck_error_context(
            "Receiving \"artifact entries for materialized paths\" future from command thread.",
        )?;

        Ok(result)
    }
}

impl DeferredMaterializerAccessor<DefaultIoHandler> {
    /// Spawns two threads (`materialization_loop` and `command_loop`).
    /// Creates and returns a new `DeferredMaterializer` that aborts those
    /// threads when dropped.
    pub fn new(
        fs: ProjectRoot,
        digest_config: DigestConfig,
        buck_out_path: ProjectRelativePathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        io_executor: Arc<dyn BlockingExecutor>,
        configs: DeferredMaterializerConfigs,
        sqlite_db: Option<MaterializerStateSqliteDb>,
        sqlite_state: Option<MaterializerState>,
        http_client: HttpClient,
        daemon_dispatcher: EventDispatcher,
    ) -> buck2_error::Result<Self> {
        let (high_priority_sender, high_priority_receiver) = mpsc::unbounded_channel();
        let (low_priority_sender, low_priority_receiver) = mpsc::unbounded_channel();

        let counters = MaterializerCounters::leak_new();

        let command_sender = Arc::new(MaterializerSender {
            high_priority: high_priority_sender,
            low_priority: low_priority_sender,
            counters,
            clean_guard: RwLock::new(None),
        });

        let command_receiver = MaterializerReceiver {
            high_priority: high_priority_receiver,
            low_priority: low_priority_receiver,
            counters,
        };

        let stats = Arc::new(DeferredMaterializerStats::default());

        let num_entries_from_sqlite = sqlite_state.as_ref().map_or(0, |s| s.len()) as u64;
        let materializer_state_info = buck2_data::MaterializerStateInfo {
            num_entries_from_sqlite,
        };
        let access_times_buffer =
            (!matches!(configs.update_access_times, AccessTimesUpdates::Disabled))
                .then(HashSet::new);

        let tree = ArtifactTree::initialize(sqlite_state);

        let io = Arc::new(DefaultIoHandler::new(
            fs,
            digest_config,
            buck_out_path,
            re_client_manager,
            io_executor,
            http_client,
        ));

        let command_processor = {
            let command_sender = command_sender.dupe();
            let rt = Handle::current();
            let stats = stats.dupe();
            let io = io.dupe();
            move |cancellations| {
                DeferredMaterializerCommandProcessor::new(
                    io,
                    sqlite_db,
                    rt,
                    configs.defer_write_actions,
                    command_sender,
                    tree,
                    cancellations,
                    stats,
                    access_times_buffer,
                    configs.verbose_materializer_log,
                    daemon_dispatcher,
                    configs.disable_eager_write_dispatch,
                )
            }
        };

        let access_time_update_max_buffer_size = access_time_update_max_buffer_size()?;

        let command_thread = thread_spawn("buck2-dm", {
            move || {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap();

                let cancellations = CancellationContext::never_cancelled();

                rt.block_on(command_processor(cancellations).run(
                    command_receiver,
                    configs.ttl_refresh,
                    access_time_update_max_buffer_size,
                    configs.update_access_times,
                    configs.clean_stale_config,
                ));
            }
        })
        .buck_error_context("Cannot start materializer thread")?;

        Ok(Self {
            command_thread: Some(command_thread),
            command_sender,
            materialize_final_artifacts: configs.materialize_final_artifacts,
            defer_write_actions: configs.defer_write_actions,
            io,
            materializer_state_info,
            stats,
        })
    }
}

/// Wait on all futures in `futs` to finish. Return Error for first future that failed
/// in the Vec.
async fn join_all_existing_futs(
    existing_futs: Vec<(ProjectRelativePathBuf, ProcessingFuture)>,
) -> buck2_error::Result<()> {
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
                f.await.with_buck_error_context(|| {
                    format!(
                        "Error waiting for a previous future to finish cleaning output path {path}"
                    )
                })?;
            }
        };
    }

    Ok(())
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct WriteFile {
    #[derivative(Debug = "ignore")]
    compressed_data: Box<[u8]>,
    decompressed_size: usize,
    is_executable: bool,
}
