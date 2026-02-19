/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::soft_error;
use buck2_directory::directory::directory_ref::DirectoryRef;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::materialize::materializer::ArtifactNotMaterializedReason;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::CopiedArtifact;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use buck2_execute::output_size::OutputSize;
use chrono::DateTime;
use chrono::Utc;
use derive_more::Display;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::Shared;
use tracing::instrument;

use crate::materializers::deferred::SharedMaterializingError;
use crate::materializers::deferred::WriteFile;
use crate::materializers::deferred::directory_metadata::DirectoryMetadata;
use crate::materializers::deferred::file_tree::FileTree;
use crate::sqlite::materializer_db::MaterializerState;
use crate::sqlite::materializer_db::MaterializerStateEntry;
use crate::sqlite::materializer_db::MaterializerStateSqliteDb;

/// A future that is materializing on a separate task spawned by the materializer
pub(crate) type MaterializingFuture =
    Shared<BoxFuture<'static, Result<(), SharedMaterializingError>>>;
/// A future that is cleaning paths on a separate task spawned by the materializer
pub(crate) type CleaningFuture = Shared<BoxFuture<'static, buck2_error::Result<()>>>;

#[derive(Clone)]
pub(crate) enum ProcessingFuture {
    Materializing(MaterializingFuture),
    Cleaning(CleaningFuture),
}

/// Tree that stores materialization data for each artifact. Used internally by
/// the `DeferredMaterializer` to keep track of artifacts and how to
/// materialize them.
pub(crate) type ArtifactTree = FileTree<Box<ArtifactMaterializationData>>;

/// The Version of a processing future associated with an artifact. We use this to know if we can
/// clear the processing field when a callback is received, or if more work is expected.
#[derive(Eq, PartialEq, Copy, Clone, Dupe, Debug, Ord, PartialOrd, Display)]
pub struct Version(pub u64);

pub struct ArtifactMaterializationData {
    /// Taken from `deps` of `ArtifactValue`. Used to materialize deps of the artifact.
    pub(crate) deps: Option<ActionSharedDirectory>,
    pub(crate) stage: ArtifactMaterializationStage,
    /// An optional future that may be processing something at the current path
    /// (for example, materializing or deleting). Any other future that needs to process
    /// this path would need to wait on the existing future to finish.
    /// TODO(scottcao): Turn this into a queue of pending futures.
    pub(crate) processing: Processing,
}

/// Represents a processing future + the version at which it was issued. When receiving
/// notifications about processing futures that finish, their changes are only applied if their
/// version is greater than the current version.
///
/// The version is an internal counter that is shared between the current processing_fut and
/// this data. When multiple operations are queued on a ArtifactMaterializationData, this
/// allows us to identify which one is current.
pub(crate) enum Processing {
    Done(Version),
    Active {
        future: ProcessingFuture,
        version: Version,
    },
}

impl Processing {
    pub(crate) fn current_version(&self) -> Version {
        match self {
            Self::Done(version) => *version,
            Self::Active { version, .. } => *version,
        }
    }

    fn into_future(self) -> Option<ProcessingFuture> {
        match self {
            Self::Done(..) => None,
            Self::Active { future, .. } => Some(future),
        }
    }
}

/// Metadata used to identify an artifact entry and stored for every materialized artifact.
/// For directory entries it might only store their fingerprints for optimization purposes.
/// For everything else (files, symlinks, and external symlinks), we use `ActionDirectoryMember`
/// as is.
#[derive(Clone, Dupe, Debug, Display)]
pub struct ArtifactMetadata(pub(crate) ActionDirectoryEntry<DirectoryMetadata>);

impl ArtifactMetadata {
    pub(crate) fn matches_entry(
        &self,
        entry: &ActionDirectoryEntry<ActionSharedDirectory>,
    ) -> bool {
        match (&self.0, entry) {
            (DirectoryEntry::Dir(d1), DirectoryEntry::Dir(d2)) => {
                d1.fingerprint() == d2.fingerprint()
            }
            (DirectoryEntry::Leaf(l1), DirectoryEntry::Leaf(l2)) => {
                // In Windows, the 'executable bit' absence can cause Buck2 to re-download identical artifacts.
                // To avoid this, we exclude the executable bit from the comparison.
                if cfg!(windows) {
                    if let (
                        ActionDirectoryMember::File(meta1),
                        ActionDirectoryMember::File(meta2),
                    ) = (l1, l2)
                    {
                        return meta1.digest == meta2.digest;
                    }
                }
                l1 == l2
            }
            _ => false,
        }
    }

    pub(crate) fn new(entry: &ActionDirectoryEntry<ActionSharedDirectory>, compact: bool) -> Self {
        let new_entry = match entry {
            DirectoryEntry::Dir(dir) => {
                let metadata = if compact {
                    DirectoryMetadata::Compact {
                        fingerprint: dir.fingerprint().dupe(),
                        total_size: entry.calc_output_count_and_bytes().bytes,
                    }
                } else {
                    DirectoryMetadata::Full(dir.dupe())
                };
                DirectoryEntry::Dir(metadata)
            }
            DirectoryEntry::Leaf(leaf) => DirectoryEntry::Leaf(leaf.dupe()),
        };
        Self(new_entry)
    }

    pub(crate) fn size(&self) -> u64 {
        match &self.0 {
            DirectoryEntry::Dir(dir) => dir.size(),
            DirectoryEntry::Leaf(ActionDirectoryMember::File(file_metadata)) => {
                file_metadata.digest.size()
            }
            DirectoryEntry::Leaf(_) => 0,
        }
    }
}

pub enum ArtifactMaterializationStage {
    /// The artifact was declared, but the materialization hasn't started yet.
    /// If it did start but end with an error, it returns to this stage.
    /// When the artifact is declared, we spawn a deletion future to delete
    /// all existing paths that conflict with the output paths.
    Declared {
        /// Taken from `entry` of `ArtifactValue`. Used to materialize the actual artifact.
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        method: Arc<ArtifactMaterializationMethod>,
        persist_full_directory_structure: bool,
    },
    /// This artifact was materialized
    Materialized {
        /// Once the artifact is materialized, we don't need the full entry anymore.
        /// We can throw away most of the entry and just keep some metadata used to
        /// check if materialized artifact matches declared artifact.
        metadata: ArtifactMetadata,
        /// Used to clean older artifacts from buck-out.
        last_access_time: DateTime<Utc>,
        /// Artifact declared by running daemon.
        /// Should not be deleted without invalidating DICE nodes, which currently
        /// means killing the daemon.
        active: bool,
    },
}

/// Different ways to materialize the files of an artifact. Some artifacts need
/// to be fetched from the CAS, others copied locally.
#[derive(Debug, Display)]
pub enum ArtifactMaterializationMethod {
    /// The files must be copied from a local path.
    #[display("local copy")]
    LocalCopy(
        /// A map `[dest => src]`, meaning that a file at
        /// `{artifact_path}/{dest}/{p}` needs to be copied from `{src}/{p}`.
        FileTree<ProjectRelativePathBuf>,
        /// Raw list of copied artifacts, as received in `declare_copy`.
        Vec<CopiedArtifact>,
    ),

    #[display("write")]
    Write(Arc<WriteFile>),

    /// The files must be fetched from the CAS.
    #[display("cas download (action: {})", info.origin)]
    CasDownload {
        /// The digest of the action that produced this output
        info: Arc<CasDownloadInfo>,
    },

    /// The file must be fetched over HTTP.
    #[display("http download ({})", info)]
    HttpDownload { info: HttpDownloadInfo },

    #[cfg(test)]
    Test,
}

pub(crate) trait MaterializationMethodToProto {
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
            #[cfg(test)]
            ArtifactMaterializationMethod::Test => unimplemented!(),
        }
    }
}

impl ArtifactTree {
    pub(crate) fn initialize(sqlite_state: Option<MaterializerState>) -> Self {
        let mut tree = ArtifactTree::new();
        if let Some(sqlite_state) = sqlite_state {
            for entry in sqlite_state.into_iter() {
                let MaterializerStateEntry {
                    path,
                    metadata,
                    last_access_time,
                } = entry;
                tree.insert(
                    path.iter().map(|f| f.to_owned()),
                    Box::new(ArtifactMaterializationData {
                        deps: None,
                        stage: ArtifactMaterializationStage::Materialized {
                            metadata,
                            last_access_time,
                            active: false,
                        },
                        processing: Processing::Done(Version(0)),
                    }),
                );
            }
        }
        tree
    }

    /// Given a path that's (possibly) not yet materialized, returns the path
    /// `contents_path` where its contents can be found. Returns Err if the
    /// contents cannot be found (ex. if it requires HTTP or CAS download)
    ///
    /// Note that the returned `contents_path` could be the same as `path`.
    #[instrument(level = "trace", skip(self), fields(path = %path))]
    pub(crate) fn file_contents_path(
        &self,
        path: ProjectRelativePathBuf,
        digest_config: DigestConfig,
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
            ArtifactMaterializationStage::Declared {
                entry,
                method,
                persist_full_directory_structure: _,
            } => (entry.dupe(), method.dupe()),
        };
        match method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { info } => {
                let path_iter = path_iter.peekable();

                let root_entry: ActionDirectoryEntry<ActionSharedDirectory> = entry.dupe();
                let mut entry = Some(entry.as_ref());

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
                            .map_dir(|d| {
                                d.as_dyn()
                                    .to_builder()
                                    .fingerprint(digest_config.as_directory_serializer())
                            })
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
                        None => self.file_contents_path(src_path.clone(), digest_config),
                        // This is not supposed to be reachable, and if it's, there
                        // is a bug somewhere else. Panic to prevent the bug from
                        // propagating.
                        Some(part) => panic!(
                            "While getting materialized path of {path:?}: path {src_path:?} is a file, so subpath {part:?} doesn't exist within.",
                        ),
                    },
                }
            }
            #[cfg(test)]
            ArtifactMaterializationMethod::Test => unimplemented!(),
        }
    }

    #[instrument(level = "debug", skip(self, result), fields(path = %artifact_path))]
    pub(crate) fn cleanup_finished(
        &mut self,
        artifact_path: ProjectRelativePathBuf,
        version: Version,
        result: Result<(), SharedMaterializingError>,
    ) {
        match self
            .prefix_get_mut(&mut artifact_path.iter())
            .ok_or_else(|| internal_error!("Path is vacant"))
        {
            Ok(info) => {
                if info.processing.current_version() > version {
                    // We can only unset the future if version matches.
                    // Otherwise, we may be unsetting a different future from a newer version.
                    tracing::debug!("version conflict");
                    return;
                }

                if result.is_err() {
                    // Leave it alone, don't keep retrying.
                } else {
                    info.processing = Processing::Done(version);
                }
            }
            Err(e) => {
                // NOTE: This shouldn't normally happen?
                soft_error!("cleanup_finished_vacant", e, quiet: true).unwrap();
            }
        }
    }

    /// Removes paths from tree and returns a pair of two vecs.
    /// First vec is a list of paths removed. Second vec is a list of
    /// pairs of removed paths to futures that haven't finished.
    pub(crate) fn invalidate_paths_and_collect_futures(
        &mut self,
        paths: Vec<ProjectRelativePathBuf>,
        sqlite_db: Option<&mut MaterializerStateSqliteDb>,
    ) -> buck2_error::Result<Vec<(ProjectRelativePathBuf, ProcessingFuture)>> {
        let mut invalidated_paths = Vec::new();
        let mut futs = Vec::new();

        for path in paths {
            for (path, data) in self.remove_path(&path) {
                if let Some(processing_fut) = data.processing.into_future() {
                    futs.push((path.clone(), processing_fut));
                }
                invalidated_paths.push(path);
            }
        }

        #[cfg(test)]
        {
            use buck2_error::buck2_error;
            for path in &invalidated_paths {
                if path.as_str() == "test/invalidate/failure" {
                    return Err(buck2_error!(buck2_error::ErrorTag::Tier0, "Injected error"));
                }
            }
        }

        // We can invalidate the paths here even if materializations are currently running on
        // the underlying nodes, because when materialization finishes we'll check the version
        // number.
        if let Some(sqlite_db) = sqlite_db {
            sqlite_db
                .materializer_state_table()
                .delete(invalidated_paths)
                .buck_error_context("Error invalidating paths in materializer state")?;
        }

        Ok(futs)
    }
}
