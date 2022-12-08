/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::file_ops::FileMetadata;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::project::ProjectRelativePathBuf;
use chrono::DateTime;
use chrono::Utc;
use derive_more::Display;
use dice::UserComputationData;
use futures::stream::BoxStream;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use thiserror::Error;

use crate::artifact_value::ArtifactValue;
use crate::base_deferred_key::BaseDeferredKey;
use crate::directory::ActionDirectoryEntry;
use crate::directory::ActionDirectoryMember;
use crate::directory::ActionImmutableDirectory;
use crate::directory::ActionSharedDirectory;
use crate::execute::action_digest::TrackedActionDigest;
#[cfg(any(fbcode_build, cargo_internal_build))]
use crate::materialize::eden_api::EdenBuckOut;
use crate::materialize::http::Checksum;

// Add a stub EdenBuckOut for when we don't have Eden output enabled
#[cfg(not(any(fbcode_build, cargo_internal_build)))]
pub struct EdenBuckOut {
    not_implemented: !,
}

#[cfg(not(any(fbcode_build, cargo_internal_build)))]
impl EdenBuckOut {
    pub async fn remove_paths_recursive(
        &self,
        _project_fs: &buck2_core::fs::project::ProjectRoot,
        _paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<()> {
        self.not_implemented
    }

    pub async fn set_path_object_id(
        &self,
        __path: &ProjectRelativePathBuf,
        __value: &ArtifactValue,
    ) -> anyhow::Result<()> {
        self.not_implemented
    }
}

pub struct WriteRequest {
    pub path: ProjectRelativePathBuf,
    pub content: Vec<u8>,
    pub is_executable: bool,
}

#[derive(Error, Debug)]
pub enum MaterializationError {
    #[error("Error materializing artifact at path `{}`", .path)]
    Error {
        path: ProjectRelativePathBuf,

        #[source]
        source: anyhow::Error,
    },

    /// The artifact wasn't found. This typically means it expired in the CAS.
    #[error(
        "Your build requires materializing an artifact that has expired in the \
        RE CAS and Buck does not have it (path: {}, digest info: {}). \
        This likely happened because your Buck daemon \
        has been online for a long time. This error is currently unrecoverable. \
        To proceed, you should restart Buck using `buck2 killall`.",
        .path,
        .info
    )]
    NotFound {
        path: ProjectRelativePathBuf,
        info: Arc<CasDownloadInfo>,
    },

    #[error("Error inserting entry into materializer state sqlite for artifact at `{}`", .path)]
    SqliteDbError {
        path: ProjectRelativePathBuf,

        #[source]
        source: anyhow::Error,
    },
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
    /// Declare that a set of artifacts exist on disk already.
    async fn declare_existing(
        &self,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()>;

    async fn declare_copy_impl(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> anyhow::Result<()>;

    async fn declare_cas_many_impl<'a, 'b>(
        &self,
        info: Arc<CasDownloadInfo>,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()>;

    async fn declare_http(
        &self,
        path: ProjectRelativePathBuf,
        info: HttpDownloadInfo,
    ) -> anyhow::Result<()>;

    /// Write contents to paths. The output is ordered in the same order as the input. Implicitly
    /// cleans up paths that the WriteRequest declares.
    async fn declare_write<'a>(
        &self,
        gen: Box<dyn FnOnce() -> anyhow::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> anyhow::Result<Vec<ArtifactValue>>;

    /// Ask the materializer if the artifacts at the set of paths match what is on disk or
    /// declared. Returns Ok(Ok) if they do and Ok(Err) if they don't. It's a result not a boolean
    /// so you can't ignore it.
    async fn declare_match(
        &self,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<DeclareMatchOutcome>;

    /// Declare an artifact at `path` exists. This will overwrite any pre-existing materialization
    /// methods for this file and indicate that no materialization is necessary.
    async fn invalidate(&self, path: ProjectRelativePathBuf) -> anyhow::Result<()> {
        self.invalidate_many(vec![path]).await
    }

    /// Declare an artifact at `path` exists. This will overwrite any pre-existing materialization
    /// methods for this file and indicate that no materialization is necessary.
    async fn invalidate_many(&self, paths: Vec<ProjectRelativePathBuf>) -> anyhow::Result<()>;

    /// Materialize artifacts paths. Returns a Stream with each element corresponding to one of the
    /// input paths, in the order that they were passed. This method provides access to
    /// individual results, but code that doesn't care about why an individual materialization
    /// failed should probably use `ensure_materialized` instead.
    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<BoxStream<'static, Result<(), MaterializationError>>>;

    /// Given a list of artifact paths, blocks until all previously declared
    /// artifacts on that list are materialized. An [`Err`] is returned if the
    /// materialization fails for one or more of these paths.
    ///
    /// Paths that weren't previously declared are not materialized and no
    /// errors are raised for them.
    async fn ensure_materialized(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<()> {
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
    ) -> anyhow::Result<bool>;

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
    ) -> anyhow::Result<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>>;

    /// Expose Eden based buck-out if the materializer is Eden
    /// Return None if not based on Eden.
    fn eden_buck_out(&self) -> Option<&EdenBuckOut> {
        None
    }

    fn as_deferred_materializer_extension(&self) -> Option<&dyn DeferredMaterializerExtensions> {
        None
    }
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
    ) -> anyhow::Result<()> {
        self.check_declared_external_symlink(&value)?;
        self.declare_copy_impl(path, value, srcs).await
    }

    /// Declares a list of artifacts whose files can be materialized by
    /// downloading from the CAS.
    pub async fn declare_cas_many<'a, 'b>(
        &self,
        info: Arc<CasDownloadInfo>,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        for (_, value) in artifacts.iter() {
            self.check_declared_external_symlink(value)?;
        }
        self.declare_cas_many_impl(info, artifacts).await
    }

    /// External symlink is a hack used to resolve the symlink to the correct external hack.
    /// No external symlink should be declared on the materializer with a non-empty remaining
    /// path. This function runs a check on all declared artifacts and returns `Err` if they
    /// are external symlinks with an existing value on `remaining_path` and `Ok` otherwise.
    fn check_declared_external_symlink(&self, value: &ArtifactValue) -> anyhow::Result<()> {
        match value.entry() {
            DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(external_symlink)) => {
                match external_symlink.remaining_path() {
                    Some(_) => {
                        return Err(anyhow::anyhow!(
                            "Internal error: external symlink should not be declared on materializer with non-empty remaining path: '{}'",
                            external_symlink.dupe().to_path_buf().display()
                        ));
                    }
                    None => {}
                }
            }
            _ => {}
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
}

impl CopiedArtifact {
    pub fn new(
        src: ProjectRelativePathBuf,
        dest: ProjectRelativePathBuf,
        dest_entry: ActionDirectoryEntry<ActionImmutableDirectory>,
    ) -> Self {
        Self {
            src,
            dest,
            dest_entry,
        }
    }
}

#[derive(Debug, Display)]
enum CasDownloadInfoOrigin {
    /// Declared by an action that executed on RE.
    #[display(
        fmt = "{} retrieved {:.3} seconds ago with ttl = {:.3} seconds",
        "action_digest",
        "action_instant.elapsed().as_secs_f64()",
        "ttl.as_secs_f64()"
    )]
    Execution {
        /// Digest of the action that led us to discover this CAS object.
        action_digest: TrackedActionDigest,

        /// When did we learn of the connection between this digest and the download it allows. This
        /// typically represents how much time has passed since we executed the action or hit in the
        /// action cache.
        action_instant: Instant,

        /// The TTL we retrieved from RE for this action.
        ttl: Duration,
    },

    /// Simply declared by an action.
    #[display(fmt = "declared")]
    Declared,
}

/// Information about a CAS download we might require when an artifact is not materialized.
#[derive(Debug, Display)]
#[display(fmt = "{}", "self.origin")]
pub struct CasDownloadInfo {
    origin: CasDownloadInfoOrigin,
    /// RE Use case to use whne downloading this
    pub re_use_case: RemoteExecutorUseCase,
}

impl CasDownloadInfo {
    pub fn new_execution(
        action_digest: TrackedActionDigest,
        re_use_case: RemoteExecutorUseCase,
        action_instant: Instant,
        ttl: Duration,
    ) -> Self {
        Self {
            origin: CasDownloadInfoOrigin::Execution {
                action_digest,
                action_instant,
                ttl,
            },
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
        match self.origin {
            CasDownloadInfoOrigin::Execution { action_instant, .. } => {
                Some(action_instant.elapsed())
            }
            CasDownloadInfoOrigin::Declared => None,
        }
    }

    pub fn action_digest(&self) -> Option<&TrackedActionDigest> {
        match &self.origin {
            CasDownloadInfoOrigin::Execution { action_digest, .. } => Some(action_digest),
            CasDownloadInfoOrigin::Declared => None,
        }
    }
}

/// Information about a CAS download we might require when an artifact is not materialized.
#[derive(Debug, Display)]
#[display(fmt = "{} declared by {}", "self.url", "self.owner")]
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

#[derive(Debug, Error)]
pub enum ArtifactNotMaterializedReason {
    #[error(
        "The artifact at path '{}' ({}) was produced by a RE action ({}), \
        but has not been downloaded",
        .path,
        .entry,
        .info
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
        .info
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
    /// Materialize all immediately as they are declared
    Immediate,
    /// Materialize only when needed
    Deferred,
    /// Materialize only when needed, do not materialize final artifacts
    DeferredSkipFinalArtifacts,
    /// Let Eden delegate materialzation
    Eden,
}

#[derive(Debug, Error)]
pub enum MaterializationMethodError {
    #[error(
        "Invalid value for buckconfig `[buck2] materializations`. Got `{0}`. Expected one of `all`, `deferred`, `deferred_skip_final_artifacts` or `eden`."
    )]
    InvalidValueForConfig(String),
}

impl MaterializationMethod {
    pub fn try_new_from_config(legacy_config: Option<&LegacyBuckConfig>) -> anyhow::Result<Self> {
        Self::try_new_from_config_value(
            legacy_config.and_then(|c| c.get("buck2", "materializations")),
        )
    }

    fn try_new_from_config_value(config_value: Option<&str>) -> anyhow::Result<Self> {
        match config_value {
            None | Some("") | Some("all") => Ok(MaterializationMethod::Immediate),
            Some("deferred") => Ok(MaterializationMethod::Deferred),
            Some("deferred_skip_final_artifacts") => {
                Ok(MaterializationMethod::DeferredSkipFinalArtifacts)
            }
            Some("eden") => Ok(MaterializationMethod::Eden),
            Some(v) => Err(MaterializationMethodError::InvalidValueForConfig(v.to_owned()).into()),
        }
    }
}

/// This trait provides a level of indirection since the concrete implementation of
/// `DeferredMaterializerEntry` lives in a crate that depends on this one.
pub trait DeferredMaterializerEntry: Send + Sync + std::fmt::Display {}

/// Extensions to the Materializer trait that are only available in the Deferred materializer.
#[async_trait]
pub trait DeferredMaterializerExtensions: Send + Sync {
    fn iterate(
        &self,
    ) -> anyhow::Result<
        BoxStream<'static, (ProjectRelativePathBuf, Box<dyn DeferredMaterializerEntry>)>,
    >;

    async fn refresh_ttls(&self, min_ttl: i64) -> anyhow::Result<()>;

    async fn clean_stale_artifacts(
        &self,
        keep_since_time: DateTime<Utc>,
        dry_run: bool,
    ) -> anyhow::Result<String>;

    fn queue_size(&self) -> usize;
}
