/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::ordered_entry_walk;
use buck2_events::dispatch::EventDispatcher;
use chrono::DateTime;
use chrono::Duration;
use chrono::Utc;
use derive_more::Display;
use dice::UserComputationData;
use dupe::Dupe;
use futures::stream::BoxStream;
use futures::stream::TryStreamExt;

use crate::artifact_value::ArtifactValue;
use crate::directory::ActionDirectoryEntry;
use crate::directory::ActionDirectoryMember;
use crate::directory::ActionImmutableDirectory;
use crate::directory::ActionSharedDirectory;
use crate::execute::action_digest::TrackedActionDigest;
use crate::materialize::http::Checksum;

pub struct WriteRequest {
    pub path: ProjectRelativePathBuf,
    pub content: Vec<u8>,
    pub is_executable: bool,
}

#[cold]
fn format_directory_entry_leaves(
    directory: &ActionDirectoryEntry<ActionSharedDirectory>,
) -> String {
    let walk = ordered_entry_walk(directory.as_ref());
    let only_files = walk
        .filter_map(|entry| match entry {
            DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => Some(&f.digest),
            _ => {
                // We only download files from RE, not symlinks or directories.
                // https://fburl.com/code/3o8ht6b6.
                None
            }
        })
        .with_paths();
    const MAX_COUNT: usize = 10;
    const TABULATION: &str = "    ";
    let mut count = 0;
    let mut result = String::new();
    for (path, digest) in only_files {
        count += 1;
        if count > MAX_COUNT {
            continue;
        }
        result.push_str(&format!("{TABULATION}{path}: {digest}\n"));
    }
    if count > MAX_COUNT {
        result.push_str(&format!(
            "{}... and {} more omitted",
            TABULATION,
            count - MAX_COUNT
        ));
    }
    result
}

#[derive(buck2_error::Error, Debug, Clone, Dupe)]
#[error(
    "Your build requires materializing an artifact that has expired in the \
    RE CAS and Buck does not have it. \
    This likely happened because your Buck daemon \
    has been online for a long time. This error is currently unrecoverable. \
    To proceed, you should restart Buck using `buck2 killall`.

Debug information:
  Path: {}
  Digest origin: {}
  Directory:\n{}", .path, .info.origin.as_display_for_not_found(), format_directory_entry_leaves(.directory))]
#[buck2(tag = MaterializationError)]
pub struct CasNotFoundError {
    pub path: Arc<ProjectRelativePathBuf>,
    pub info: Arc<CasDownloadInfo>,
    pub directory: ActionDirectoryEntry<ActionSharedDirectory>,
    #[source]
    pub error: Arc<buck2_error::Error>,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = MaterializationError)]
pub enum MaterializationError {
    #[error("Error materializing artifact at path `{}`", .path)]
    Error {
        path: ProjectRelativePathBuf,

        #[source]
        source: buck2_error::Error,
    },

    /// The artifact wasn't found. This typically means it expired in the CAS.
    #[error(transparent)]
    NotFound { source: CasNotFoundError },

    #[error("Error inserting entry into materializer state sqlite for artifact at `{}`", .path)]
    SqliteDbError {
        path: ProjectRelativePathBuf,

        #[source]
        source: buck2_error::Error,
    },
}

#[derive(Debug)]
pub struct DeclareArtifactPayload {
    pub path: ProjectRelativePathBuf,
    pub artifact: ArtifactValue,
    /// Whether materializer state should store full information about directory artifact.
    pub persist_full_directory_structure: bool,
}

/// A trait providing methods to asynchronously materialize artifacts.
///
/// # Invariants
///
/// 0. Before accessing an artifact on disk, `ensure_materialized` must be
///    called with its path. Alternatively, `get_materialized_file_paths` can
///    be used to *try* to find an already materialized path of a file with the
///    same contents as the desired path, if the contents are relevant but the
///    path isn't.
///
/// 1. If an artifact is declared twice followed by `ensure_materialized`, the
///    latest declaration is the one guaranteed to have been materialized.
///    However, if an external source modifies the artifact on disk after it's
///    been declared, there are no guarantees that after `ensure_materialized`
///    the declared artifact will be on disk (due to race conditions).
///
/// 2. If `declare_*` is called concurrently for two artifacts with conflicting
///    paths or subpaths, there are no guarantees on which of the declarations
///    will be used when materializing - possibly a mix of both.
///
/// 3. If `ensure_materialized` is called on a path that wasn't previously
///    declared, that path is not materialized and no errors are raised for it.
///
/// 4. Declare may delete any existing paths that conflict with the path that was
///    declared.
#[async_trait]
pub trait Materializer: Allocative + Send + Sync + 'static {
    /// The name of this materializer, for logging.
    fn name(&self) -> &str;

    /// Declare that a set of artifacts exist on disk already.
    async fn declare_existing(
        &self,
        artifacts: Vec<DeclareArtifactPayload>,
    ) -> buck2_error::Result<()>;

    async fn declare_copy_impl(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> buck2_error::Result<()>;

    async fn declare_cas_many_impl<'a, 'b>(
        &self,
        info: Arc<CasDownloadInfo>,
        artifacts: Vec<DeclareArtifactPayload>,
    ) -> buck2_error::Result<()>;

    async fn declare_http(
        &self,
        path: ProjectRelativePathBuf,
        info: HttpDownloadInfo,
    ) -> buck2_error::Result<()>;

    /// Write contents to paths. The output is ordered in the same order as the input. Implicitly
    /// cleans up paths that the WriteRequest declares.
    async fn declare_write<'a>(
        &self,
        generate: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> buck2_error::Result<Vec<ArtifactValue>>;

    /// Ask the materializer if the artifacts at the set of paths match what is on disk or
    /// declared. Returns Ok(Ok) if they do and Ok(Err) if they don't. It's a result not a boolean
    /// so you can't ignore it.
    async fn declare_match(
        &self,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> buck2_error::Result<DeclareMatchOutcome>;

    /// Ask the materializer if there is a "tracked" artifact at the given path.
    ///
    /// While this method provides no information about what that artifact actually is, it can be
    /// used to verify that the artifact has not been eg corrupted by a partial clean-stale since
    /// being materialized.
    ///
    /// Furthermore, if this method returns `true`, the artifact is also treated as having been
    /// declared in the current daemon.
    ///
    /// This method does not guarantee that the artifact was materialized.
    async fn has_artifact_at(&self, path: ProjectRelativePathBuf) -> buck2_error::Result<bool>;

    /// Declare an artifact at `path` exists. This will overwrite any pre-existing materialization
    /// methods for this file and indicate that no materialization is necessary.
    async fn invalidate(&self, path: ProjectRelativePathBuf) -> buck2_error::Result<()> {
        self.invalidate_many(vec![path]).await
    }

    /// Declare an artifact at `path` exists. This will overwrite any pre-existing materialization
    /// methods for this file and indicate that no materialization is necessary.
    async fn invalidate_many(&self, paths: Vec<ProjectRelativePathBuf>) -> buck2_error::Result<()>;

    /// Materialize artifacts paths. Returns a Stream with each element corresponding to one of the
    /// input paths, in the order that they were passed. This method provides access to
    /// individual results, but code that doesn't care about why an individual materialization
    /// failed should probably use `ensure_materialized` instead.
    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<BoxStream<'static, Result<(), MaterializationError>>>;

    /// Given a list of artifact paths, blocks until all previously declared
    /// artifacts on that list are materialized. An [`Err`] is returned if the
    /// materialization fails for one or more of these paths.
    ///
    /// Paths that weren't previously declared are not materialized and no
    /// errors are raised for them.
    async fn ensure_materialized(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<()> {
        Ok(self
            .materialize_many(artifact_paths)
            .await?
            .try_collect()
            .await?)
    }

    /// Similar to `ensure_materialized`, but it relaxes its most important
    /// invariant: there's no guarantee that the artifact will be materialized
    /// after calling this method. It's meant for final artifacts that are NOT
    /// required by further build steps and therefore can be skipped in some
    /// cases.
    ///
    /// The materializer returns `Ok(true)` if the materialization succeeded,
    /// `Ok(false)` if it was skipped, and [`Err`] if it was tried but failed.
    ///
    /// Calling this on an artifact that was never declared is undefined
    /// behavior.
    async fn try_materialize_final_artifact(
        &self,
        artifact_path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<bool>;

    /// Given a `file_path` whose contents we are interested in, *tries* to
    /// find a materialized path with the same contents. It returns [`None`] if
    /// the path leads to a file that needs to be fetched from the CAS.
    ///
    /// If none of the declared artifacts contains `file_path`, the
    /// materializer can't tell whether that's because it was materialized by
    /// an external source, or because it doesn't exist. Since this usually
    /// means the path is a source file (not an output), for simplicity the
    /// materializer returns `Some(file_path)`, not [`None`].
    ///
    /// TODO(rafaelc): analyze if we can get rid of this method without making
    /// Buck2 significantly slower, after we stop deferring local copies of
    /// artifacts that are already on disk.
    async fn get_materialized_file_paths(
        &self,
        file_paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>>;

    fn as_deferred_materializer_extension(&self) -> Option<&dyn DeferredMaterializerExtensions> {
        None
    }

    /// Currently no-op for all materializers except deferred materializer
    fn log_materializer_state(&self, _events: &EventDispatcher) {}

    /// Inject stats into a snapshot. This is also used only for the deferred materializer at this
    /// time.
    fn add_snapshot_stats(&self, _snapshot: &mut buck2_data::Snapshot) {}

    /// Given a list of `paths`, returns a list of corresponding artifact entries only if all the following conditions are met:
    ///   - There is an artifact at the given path (either declared or materialized).
    ///   - The materializer state contains sufficient information about the artifact.
    ///   - The path refers to the root of the artifact (not a subpath).
    /// If any of these conditions are not satisfied for a given path, `None` is returned for that path.
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
    >;
}

#[derive(Copy, Clone, Dupe, Debug)]
#[must_use]
pub enum DeclareMatchOutcome {
    Match,
    NotMatch,
}

impl From<bool> for DeclareMatchOutcome {
    fn from(is_match: bool) -> Self {
        if is_match {
            Self::Match
        } else {
            Self::NotMatch
        }
    }
}

impl DeclareMatchOutcome {
    pub fn is_match(self) -> bool {
        match self {
            Self::Match => true,
            Self::NotMatch => false,
        }
    }
}

impl dyn Materializer {
    /// Declare an artifact at `path` whose files can be materialized by doing
    /// a local copy. Implicitly cleans up `path`.
    ///
    /// The dest of each copied artifact in `srcs` must either be equal to
    /// `path`, or be a subpath of `path`; otherwise, [`Err`] is returned.
    pub async fn declare_copy(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> buck2_error::Result<()> {
        self.check_declared_external_symlink(&value)?;
        self.declare_copy_impl(path, value, srcs).await
    }

    /// Declares a list of artifacts whose files can be materialized by
    /// downloading from the CAS.
    pub async fn declare_cas_many(
        &self,
        info: Arc<CasDownloadInfo>,
        artifacts: Vec<DeclareArtifactPayload>,
    ) -> buck2_error::Result<()> {
        for DeclareArtifactPayload {
            artifact: value, ..
        } in artifacts.iter()
        {
            self.check_declared_external_symlink(value)?;
        }
        self.declare_cas_many_impl(info, artifacts).await
    }

    /// External symlink is a hack used to resolve the symlink to the correct external hack.
    /// No external symlink should be declared on the materializer with a non-empty remaining
    /// path. This function runs a check on all declared artifacts and returns `Err` if they
    /// are external symlinks with an existing value on `remaining_path` and `Ok` otherwise.
    fn check_declared_external_symlink(&self, value: &ArtifactValue) -> buck2_error::Result<()> {
        if let DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(external_symlink)) =
            value.entry()
        {
            if !external_symlink.remaining_path().is_empty() {
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Internal error: external symlink should not be declared on materializer with non-empty remaining path: '{}'",
                    external_symlink.dupe().to_path_buf().display()
                ));
            }
        }
        Ok(())
    }
}

/// Information to perform the copy of an artifact from `src` to `dest`.
#[derive(Debug)]
pub struct CopiedArtifact {
    /// Source path of the copied artifact.
    pub src: ProjectRelativePathBuf,
    /// Destination path.
    pub dest: ProjectRelativePathBuf,
    /// Entry of the artifact at `dest`.
    pub dest_entry: ActionDirectoryEntry<ActionImmutableDirectory>,
    // Override the destination executable bit to +x (true) or -x (false)
    pub executable_bit_override: Option<bool>,
}

impl CopiedArtifact {
    pub fn new(
        src: ProjectRelativePathBuf,
        dest: ProjectRelativePathBuf,
        dest_entry: ActionDirectoryEntry<ActionImmutableDirectory>,
        executable_bit_override: Option<bool>,
    ) -> Self {
        Self {
            src,
            dest,
            dest_entry,
            executable_bit_override,
        }
    }
}

#[derive(Debug)]
pub enum CasDownloadInfoOrigin {
    /// Declared by an action that executed on RE.
    Execution(ActionExecutionOrigin),

    /// Simply declared by an action.
    Declared,
}

#[derive(Debug)]
pub struct ActionExecutionOrigin {
    /// Digest of the action that led us to discover this CAS object.
    action_digest: TrackedActionDigest,

    /// When did we learn of the connection between this digest and the download it allows. This
    /// typically represents how much time has passed since we executed the action or hit in the
    /// action cache.
    ///
    /// NOTE: we do not store this as `std::time::Instant`, because `Instant::elapsed()` does
    /// not necessarily track wall time (which is what we rather care about when dealing with
    /// TTLs received from RE). In particular, `Instant::elapsed()` can be *very* far off if
    /// the system went to sleep (on MacOS, it will not increase during that time!).
    ///
    /// See: <https://github.com/rust-lang/rust/issues/79462>
    action_instant: DateTime<Utc>,

    /// The TTL we retrieved from RE for this action.
    ttl: Duration,
}

impl ActionExecutionOrigin {
    fn action_age(&self) -> Duration {
        // NOTE: This might return a negative duration if our time skewed a lot, but that's
        // actually totally fine, since we only care to know if this is < TTL.
        Utc::now() - self.action_instant
    }
}

impl fmt::Display for CasDownloadInfoOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Execution(execution) => {
                write!(
                    f,
                    "{} retrieved {:.3} seconds ago with ttl = {:.3} seconds",
                    execution.action_digest,
                    execution.action_age().num_seconds(),
                    execution.ttl.num_seconds()
                )?;
            }
            Self::Declared => {
                write!(f, "declared")?;
            }
        }

        Ok(())
    }
}

impl CasDownloadInfoOrigin {
    pub fn as_display_for_not_found(&self) -> CasDownloadInfoOriginNotFound<'_> {
        CasDownloadInfoOriginNotFound { inner: self }
    }

    /// Does the action cache guarantee this result exist? We expect the action cache to always
    /// return a TTL that is lower than the TTL of any of the outputs referenced by the action.
    pub fn guaranteed_by_action_cache(&self) -> bool {
        match self {
            Self::Execution(execution) => execution.action_age() < execution.ttl,
            Self::Declared => {
                // If the output was just declared, then there are no action cache guarantees.
                false
            }
        }
    }
}

/// A Display wrapper for CasDownloadInfoOrigin in cases where this origin was not found (in those
/// cases we want to report potential action cache corruption).
pub struct CasDownloadInfoOriginNotFound<'a> {
    inner: &'a CasDownloadInfoOrigin,
}

impl fmt::Display for CasDownloadInfoOriginNotFound<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)?;
        if self.inner.guaranteed_by_action_cache() {
            write!(f, " (not expired: action cache corruption)")?;
        }

        Ok(())
    }
}

/// Information about a CAS download we might require when an artifact is not materialized.
#[derive(Debug, Display)]
#[display("{}, re_use_case = {}", self.origin, self.re_use_case)]
pub struct CasDownloadInfo {
    pub origin: CasDownloadInfoOrigin,
    /// RE Use case to use when downloading this
    pub re_use_case: RemoteExecutorUseCase,
}

impl CasDownloadInfo {
    pub fn new_execution(
        action_digest: TrackedActionDigest,
        re_use_case: RemoteExecutorUseCase,
        action_instant: DateTime<Utc>,
        ttl: Duration,
    ) -> Self {
        Self {
            origin: CasDownloadInfoOrigin::Execution(ActionExecutionOrigin {
                action_digest,
                action_instant,
                ttl,
            }),
            re_use_case,
        }
    }

    pub fn new_declared(re_use_case: RemoteExecutorUseCase) -> Self {
        Self {
            origin: CasDownloadInfoOrigin::Declared,
            re_use_case,
        }
    }

    pub fn action_age(&self) -> Option<Duration> {
        match &self.origin {
            CasDownloadInfoOrigin::Execution(execution) => Some(execution.action_age()),
            CasDownloadInfoOrigin::Declared => None,
        }
    }

    pub fn action_digest(&self) -> Option<&TrackedActionDigest> {
        match &self.origin {
            CasDownloadInfoOrigin::Execution(execution) => Some(&execution.action_digest),
            CasDownloadInfoOrigin::Declared => None,
        }
    }
}

/// Information about a CAS download we might require when an artifact is not materialized.
#[derive(Debug, Display)]
#[display("{} declared by {}", self.url, self.owner)]
pub struct HttpDownloadInfo {
    /// URL to download the file from.
    pub url: Arc<str>,

    /// Size, whether the file is executable. Also contains a digest, which is a bit of a shame
    /// since it's duplicative of checksum.
    pub metadata: FileMetadata,

    /// Checksum for the file, to valiate before downloading.
    pub checksum: Checksum,

    /// Target that declared the action.
    pub owner: BaseDeferredKey,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub enum ArtifactNotMaterializedReason {
    #[error(
        "The artifact at path '{}' ({}) was produced by a RE action ({}), \
        but has not been downloaded",
        .path,
        .entry,
        .info.origin.as_display_for_not_found()
    )]
    RequiresCasDownload {
        path: ProjectRelativePathBuf,
        entry: ActionDirectoryEntry<ActionImmutableDirectory>,
        info: Arc<CasDownloadInfo>,
    },

    #[error("The artifact at path '{}' has not been downloaded yet", .path)]
    RequiresMaterialization { path: ProjectRelativePathBuf },

    #[error(
        "The artifact at path '{}' points into an entry ({}) \
        produced by a RE action ({}), but that entry does not contain \
        this exact path.",
        .path,
        .entry,
        .info.origin
    )]
    DeferredMaterializerCorruption {
        path: ProjectRelativePathBuf,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        info: Arc<CasDownloadInfo>,
    },
}

// ==== dice ====

pub trait SetMaterializer {
    fn set_materializer(&mut self, materializer: Arc<dyn Materializer>);
}

pub trait HasMaterializer {
    fn get_materializer(&self) -> Arc<dyn Materializer>;
}

impl SetMaterializer for UserComputationData {
    fn set_materializer(&mut self, materializer: Arc<dyn Materializer>) {
        self.data.set(materializer);
    }
}

impl HasMaterializer for UserComputationData {
    fn get_materializer(&self) -> Arc<dyn Materializer> {
        self.data
            .get::<Arc<dyn Materializer>>()
            .expect("Materializer should be set")
            .dupe()
    }
}

#[derive(Clone, Copy, Debug, Dupe)]
pub enum MaterializationMethod {
    /// Materialize only when needed
    Deferred,
    /// Materialize only when needed, do not materialize final artifacts
    DeferredSkipFinalArtifacts,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub enum MaterializationMethodError {
    #[error(
        "Invalid value for buckconfig `[buck2] materializations`. Got `{0}`. Expected one of `all`, `deferred`, or `deferred_skip_final_artifacts`."
    )]
    InvalidValueForConfig(String),
}

impl MaterializationMethod {
    pub fn try_new_from_config_value(config_value: Option<&str>) -> buck2_error::Result<Self> {
        match config_value {
            None | Some("") | Some("deferred") => Ok(MaterializationMethod::Deferred),
            Some("deferred_skip_final_artifacts") => {
                Ok(MaterializationMethod::DeferredSkipFinalArtifacts)
            }
            Some(v) => Err(MaterializationMethodError::InvalidValueForConfig(v.to_owned()).into()),
        }
    }
}

/// This trait provides a level of indirection since the concrete implementation of
/// `DeferredMaterializerEntry` lives in a crate that depends on this one.
pub trait DeferredMaterializerEntry: Send + Sync + std::fmt::Display {}

pub struct DeferredMaterializerIterItem {
    pub artifact_path: ProjectRelativePathBuf,
    pub artifact_display: Box<dyn DeferredMaterializerEntry>,
    pub deps: Vec<(ProjectRelativePathBuf, &'static str)>,
}

/// Obtain notifications for entries as they are materialized, and request eager materialization of
/// those paths.
#[async_trait]
pub trait DeferredMaterializerSubscription: Send + Sync {
    /// Get notifications for specific paths. This also implicitly requests their eager
    /// materialization.
    fn subscribe_to_paths(&mut self, paths: Vec<ProjectRelativePathBuf>);

    /// Stop getting notifications for specific paths. In-flight notifications may still be
    /// received.
    fn unsubscribe_from_paths(&mut self, paths: Vec<ProjectRelativePathBuf>);

    /// Await the next materialization on this subscription.
    async fn next_materialization(&mut self) -> Option<ProjectRelativePathBuf>;
}

/// Extensions to the Materializer trait that are only available in the Deferred materializer.
#[async_trait]
pub trait DeferredMaterializerExtensions: Send + Sync {
    fn iterate(&self) -> buck2_error::Result<BoxStream<'static, DeferredMaterializerIterItem>>;

    fn list_subscriptions(&self)
    -> buck2_error::Result<BoxStream<'static, ProjectRelativePathBuf>>;

    /// Obtain a list of files that don't match their in-memory representation. This may not catch
    /// all discrepancies.
    fn fsck(
        &self,
    ) -> buck2_error::Result<BoxStream<'static, (ProjectRelativePathBuf, buck2_error::Error)>>;

    async fn refresh_ttls(&self, min_ttl: i64) -> buck2_error::Result<()>;

    async fn get_ttl_refresh_log(&self) -> buck2_error::Result<String>;

    async fn clean_stale_artifacts(
        &self,
        keep_since_time: DateTime<Utc>,
        dry_run: bool,
        tracked_only: bool,
    ) -> buck2_error::Result<buck2_cli_proto::CleanStaleResponse>;

    async fn test_iter(&self, count: usize) -> buck2_error::Result<String>;
    async fn flush_all_access_times(&self) -> buck2_error::Result<String>;

    /// Create a new DeferredMaterializerSubscription.
    async fn create_subscription(
        &self,
    ) -> buck2_error::Result<Box<dyn DeferredMaterializerSubscription>>;
}
