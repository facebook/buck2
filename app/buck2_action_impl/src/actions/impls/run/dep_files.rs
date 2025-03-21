/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_action_metadata_proto::DepFileInputs;
use buck2_action_metadata_proto::RemoteDepFile;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_execution_target::ActionExecutionTarget;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::impls::expanded_command_line::ExpandedCommandLineDigest;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_common::cas_digest::CasDigestData;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::buck2_env;
use buck2_core::category::Category;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathNormalizer;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::soft_error;
use buck2_directory::directory::directory_selector::DirectorySelector;
use buck2_directory::directory::fingerprinted_directory::FingerprintedDirectory;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::span_async_simple;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::expand_selector_for_dependencies;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionImmutableDirectory;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::directory::INTERNER;
use buck2_execute::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use buck2_execute::execute::action_digest_and_blobs::ActionDigestAndBlobsBuilder;
use buck2_execute::execute::cache_uploader::IntoRemoteDepFile;
use buck2_execute::execute::dep_file_digest::DepFileDigest;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::OutputType;
use buck2_execute::materialize::materializer::MaterializationError;
use buck2_execute::materialize::materializer::Materializer;
use buck2_file_watcher::dep_files::FLUSH_DEP_FILES;
use buck2_file_watcher::dep_files::FLUSH_NON_LOCAL_DEP_FILES;
use dashmap::DashMap;
use derive_more::Display;
use dupe::Dupe;
use futures::StreamExt;
use once_cell::sync::Lazy;
use parking_lot::MappedMutexGuard;
use parking_lot::Mutex;
use parking_lot::MutexGuard;
use starlark_map::ordered_map::OrderedMap;
use tracing::instrument;

#[allocative::root]
static DEP_FILES: Lazy<DashMap<DepFilesKey, Arc<DepFileState>>> = Lazy::new(DashMap::new);

/// When this is set, we retain directories after fingerprinting, so that we can output them later
/// for debugging via `buck2 audit dep-files`.
fn keep_directories() -> buck2_error::Result<bool> {
    buck2_env!("BUCK2_KEEP_DEP_FILE_DIRECTORIES", bool)
}

/// Forget about all dep files. This isn't really meant to be commonly used, but if an invalid dep
/// file was produced and the user wants unblocking, this will provide it.
fn flush_dep_files() {
    tracing::info!("Flushing all {} dep files", DEP_FILES.len());
    DEP_FILES.clear();
}

/// Flush all dep files that were not produced locally.
/// In general we may want to retain dep files that were produced locally for longer, since (a) they are
/// already on disk and we don't need to download them, and (b) since they were produced locally they are
/// not cached elsewhere so there is more value in retaining them.
fn flush_non_local_dep_files() {
    tracing::info!(
        "Flushing non-local dep files, current size is: {}",
        DEP_FILES.len()
    );
    DEP_FILES.retain(|_, dep_file_state| dep_file_state.was_produced_locally);
    tracing::info!(
        "Number of remaining local dep files is: {}",
        DEP_FILES.len()
    );
}

pub(crate) fn init_flush_dep_files() {
    FLUSH_DEP_FILES.init(flush_dep_files);
    FLUSH_NON_LOCAL_DEP_FILES.init(flush_non_local_dep_files);
}

pub(crate) fn get_dep_files(key: &DepFilesKey) -> Option<Arc<DepFileState>> {
    DEP_FILES.get(key).map(|s| s.dupe())
}

/// A key used to associate a RunAction with a possible previous dep file.
#[derive(Eq, PartialEq, Hash, Display, Allocative)]
#[display(
    "{} {} {}",
    owner,
    category,
    identifier.as_deref().unwrap_or("<no identifier>")
)]
pub(crate) struct DepFilesKey {
    owner: BaseDeferredKey,
    category: Category,
    identifier: Option<String>,
}

impl DepFilesKey {
    pub(crate) fn new(
        owner: BaseDeferredKey,
        category: Category,
        identifier: Option<String>,
    ) -> Self {
        Self {
            owner,
            category,
            identifier,
        }
    }

    pub(crate) fn from_action_execution_target(target: ActionExecutionTarget<'_>) -> Self {
        Self {
            owner: target.owner().dupe(),
            category: target.category().to_owned(),
            identifier: target.identifier().map(|t| t.to_owned()),
        }
    }
}

/// The input signatures for a DepFileState. We compute those lazily, so we either have the input
/// directories (no computation done), or the actual signatures (computation was done).
#[derive(Allocative)]
enum DepFileStateInputSignatures {
    /// Deferred(Some) means we have the input directories but haven't filtered them using the dep
    /// file yet (which in fact we haven't downloaded yet). Deferred(None) is a case that should
    /// never happen, since it would mean we panicked while producing the signatures, which is not
    /// fallible. It's the moral equivalent of a poisoned mutex.
    Deferred(Option<PartitionedInputs<ActionSharedDirectory>>),

    /// Computed represents the case where we have produced the input signatures. We only do this
    /// once at most.
    Computed(StoredFingerprints),
}

impl StoredFingerprints {
    fn to_dep_file_state(
        self,
        digests: CommandDigests,
        declared_dep_files: DeclaredDepFiles,
        result: &ActionOutputs,
        was_produced_locally: bool,
    ) -> DepFileState {
        let input_signatures = Mutex::new(DepFileStateInputSignatures::Computed(self));
        DepFileState {
            digests,
            input_signatures,
            declared_dep_files,
            result: result.dupe(),
            was_produced_locally,
        }
    }
}

#[derive(Allocative)]
pub(crate) enum StoredFingerprints {
    /// Store only digests. This is what we use in prod because it is small.
    Digests(PartitionedInputs<TrackedFileDigest>),

    /// Store digests + dirs. We allow this via BUCK2_KEEP_DEP_FILE_DIRECTORIES because it gives
    /// more debuggability.
    Dirs(PartitionedInputs<ActionImmutableDirectory>),
}

impl PartialEq<PartitionedInputs<ActionImmutableDirectory>> for StoredFingerprints {
    fn eq(&self, other: &PartitionedInputs<ActionImmutableDirectory>) -> bool {
        let fingerprints = match self {
            Self::Digests(fingerprints) => Cow::Borrowed(fingerprints),
            Self::Dirs(dirs) => Cow::Owned(dirs.as_fingerprints()),
        };

        *fingerprints == other.as_fingerprints()
    }
}

/// The state that resulted from the previous evaluation of a command that produced dep files. This
/// contains everything we need to determine whether re-evaluation is necessary (and if it isn't,
/// to return the previous value).
#[derive(Allocative)]
pub(crate) struct DepFileState {
    digests: CommandDigests,
    input_signatures: Mutex<DepFileStateInputSignatures>,
    declared_dep_files: DeclaredDepFiles,
    result: ActionOutputs,
    was_produced_locally: bool,
}

#[derive(Allocative)]
pub(crate) struct CommandDigests {
    pub(crate) cli: ExpandedCommandLineDigest,
    pub(crate) directory: FileDigest,
}

impl DepFileState {
    pub(crate) fn has_signatures(&self) -> bool {
        match *self.input_signatures.lock() {
            DepFileStateInputSignatures::Computed(..) => true,
            DepFileStateInputSignatures::Deferred(..) => false,
        }
    }

    pub(crate) fn declared_dep_files(&self) -> &DeclaredDepFiles {
        &self.declared_dep_files
    }

    /// Compute the signature for this DepFileState, having provided the dep files from
    /// read_dep_files.
    pub(crate) fn locked_compute_fingerprints<'a>(
        &'a self,
        dep_files: Cow<'_, ConcreteDepFiles>,
        keep_directories: bool,
        digest_config: DigestConfig,
    ) -> MappedMutexGuard<'a, StoredFingerprints> {
        // Now we need to know the signatures on the original action. Produce them if they're
        // missing. We're either storing input directories or outputs here.
        let mut guard = self.input_signatures.lock();

        if let DepFileStateInputSignatures::Deferred(ref mut directories) = *guard {
            let fingerprints = compute_fingerprints(
                directories
                    .take()
                    .expect("Poisoned DepFileStateInputSignatures")
                    .unshare(),
                dep_files.into_owned(),
                digest_config,
                keep_directories,
            );

            *guard = DepFileStateInputSignatures::Computed(fingerprints);
        }

        MutexGuard::map(guard, |v| match v {
            DepFileStateInputSignatures::Computed(signatures) => signatures,
            DepFileStateInputSignatures::Deferred(..) => unreachable!(),
        })
    }
}

/// The set of dep files declared by a RunAction, matching tags to their labels. We enforce at
/// creation time that tags and labels are both unique.
#[derive(Debug, Allocative)]
pub(crate) struct RunActionDepFiles {
    pub(crate) labels: OrderedMap<ArtifactTag, Arc<str>>,
}

impl Display for RunActionDepFiles {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let values: Vec<&str> = self
            .labels
            .values()
            .map(|x| x.as_ref())
            .collect::<Vec<&str>>();
        write!(f, "{:?}", values)
    }
}

impl RunActionDepFiles {
    pub(crate) fn new() -> Self {
        Self {
            labels: OrderedMap::new(),
        }
    }
}

fn get_output_path_digest(
    digest_config: DigestConfig,
    output_paths: &[(ProjectRelativePathBuf, OutputType)],
) -> CasDigestData {
    let mut digester = CasDigestData::digester(digest_config.cas_digest_config());
    digester.update(&output_paths.len().to_le_bytes());
    for (output_path, output_type) in output_paths.iter() {
        digester.update(output_path.as_str().as_bytes());
        digester.update(&[*output_type as u8]);
    }
    digester.finalize()
}

// A utility struct to hold digests that are included in both remote depfile key and value
pub(crate) struct CommonDigests {
    commandline_cli_digest: ExpandedCommandLineDigest,
    // A digest of all output paths for this action
    output_paths_digest: CasDigestData,
    // A digest of inputs that are untagged (not tied to a dep file)
    untagged_inputs_digest: TrackedFileDigest,
}
impl CommonDigests {
    // Take the digest of everythig in the structure
    fn fingerprint(&self, digest_config: DigestConfig) -> CasDigestData {
        let mut digester = CasDigestData::digester(digest_config.cas_digest_config());

        digester.update(self.output_paths_digest.raw_digest().as_bytes());
        digester.update(self.commandline_cli_digest.as_bytes());
        digester.update(self.untagged_inputs_digest.raw_digest().as_bytes());

        digester.finalize()
    }

    // Construct an action so that the action digest can be used as a remote dep file key,
    // which is the key to an action result with dep file metadata.
    // The action itself is unused but we need to upload some action to avoid permission
    // errors when uploading the action result, and the digest needs to match that action.
    fn make_remote_dep_file_action(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> ActionDigestAndBlobs {
        let digest_config = ctx.digest_config();
        let mut digester = DepFileDigest::digester(digest_config.cas_digest_config());
        digester.update(self.fingerprint(digest_config).raw_digest().as_bytes());

        // Take the digest of the mergebase to get the closest hit.
        match ctx.mergebase().0.as_ref() {
            Some(m) => digester.update(m.as_bytes()),
            None => (),
        };
        let inner_remote_dep_file_key = digester.finalize().to_string();

        let mut blobs = ActionDigestAndBlobsBuilder::new(digest_config);
        let command = blobs.add_command(&remote_execution::Command {
            // Instead of using the digest directly, we could use the constituent digests, or constituent paths
            // which might be useful for debugging.
            arguments: vec![inner_remote_dep_file_key],
            platform: Some(ctx.re_platform().clone()),
            ..Default::default()
        });

        blobs.build(&remote_execution::Action {
            command_digest: Some(command.to_grpc()),
            ..Default::default()
        })
    }

    /// Take a list of declared dep files (label, artifact) and filtered inputs (StoredFingerprints)
    /// and create a list of dep file path and filtered inputs for that input
    fn get_dep_file_inputs(
        &self,
        declared_dep_files: &DeclaredDepFiles,
        filtered_input_fingerprints: &StoredFingerprints,
    ) -> Vec<DepFileInputs> {
        let filtered_input_fingerprints = match filtered_input_fingerprints {
            StoredFingerprints::Digests(digests) => Cow::Borrowed(digests),
            StoredFingerprints::Dirs(dirs) => Cow::Owned(dirs.as_fingerprints()),
        };
        declared_dep_files
            .tagged
            .iter()
            // Both declared inputs and filtered fingerprints are constructed with preserved input ordering
            .zip(filtered_input_fingerprints.tagged.iter())
            .map(|((_, dep_file), (_, filtered_fingerprint))| DepFileInputs {
                dep_file_path: dep_file.output.get_path().to_string(),
                filtered_fingerprint: filtered_fingerprint.raw_digest().as_bytes().to_vec(),
            })
            .collect()
    }

    fn make_dep_file_entry_proto(
        &self,
        declared_dep_files: &DeclaredDepFiles,
        filtered_input_fingerprints: &StoredFingerprints,
    ) -> RemoteDepFile {
        let output_paths_digest = self.output_paths_digest.raw_digest().as_bytes().to_vec();
        let commandline_cli_digest = self.commandline_cli_digest.as_bytes().to_vec();
        let untagged_inputs_digest = self.untagged_inputs_digest.raw_digest().as_bytes().to_vec();
        let dep_file_inputs =
            self.get_dep_file_inputs(declared_dep_files, filtered_input_fingerprints);

        RemoteDepFile {
            commandline_cli_digest,
            output_paths_digest,
            untagged_inputs_digest,
            dep_file_inputs,
        }
    }
}

pub(crate) struct DepFileBundle {
    dep_files_key: DepFilesKey,
    pub(crate) remote_dep_file_action: ActionDigestAndBlobs,
    input_directory_digest: FileDigest,
    shared_declared_inputs: PartitionedInputs<ActionSharedDirectory>,
    declared_dep_files: DeclaredDepFiles,
    filtered_input_fingerprints: Option<StoredFingerprints>,
    common_digests: CommonDigests,
}

#[async_trait]
impl IntoRemoteDepFile for DepFileBundle {
    fn remote_dep_file_action(&self) -> &ActionDigestAndBlobs {
        &self.remote_dep_file_action
    }

    async fn make_remote_dep_file(
        &mut self,
        digest_config: DigestConfig,
        fs: &ArtifactFs,
        materializer: &dyn Materializer,
    ) -> buck2_error::Result<RemoteDepFile> {
        // Compute the input fingerprint digest if it hasn't been computed already.
        if self.filtered_input_fingerprints.is_none() {
            self.filtered_input_fingerprints = Some(
                eagerly_compute_fingerprints(
                    digest_config,
                    fs,
                    materializer,
                    &self.shared_declared_inputs,
                    &self.declared_dep_files,
                )
                .await?,
            );
        }

        Ok(self.common_digests.make_dep_file_entry_proto(
            &self.declared_dep_files,
            self.filtered_input_fingerprints.as_ref().unwrap(),
        ))
    }
}

impl DepFileBundle {
    pub(crate) async fn check_local_dep_file_cache_for_identical_action(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        declared_outputs: &[BuildArtifact],
    ) -> buck2_error::Result<(Option<(ActionOutputs, ActionExecutionMetadata)>, bool)> {
        // Get the action outputs (if cache hit) and an indicator on whether a full lookup operation should be performed
        let (outputs, check_filtered_inputs) = span_async_simple(
            buck2_data::MatchDepFilesStart {
                checking_filtered_inputs: false,
                remote_cache: false,
            },
            match_if_identical_action(
                ctx,
                &self.dep_files_key,
                &self.input_directory_digest,
                &self.common_digests.commandline_cli_digest,
                declared_outputs,
                &self.declared_dep_files,
            ),
            buck2_data::MatchDepFilesEnd {},
        )
        .await?;
        let outputs = outputs.map(|o| {
            (
                o,
                ActionExecutionMetadata {
                    execution_kind: ActionExecutionKind::LocalActionCache,
                    timing: Default::default(),
                    input_files_bytes: None,
                },
            )
        });
        Ok((outputs, check_filtered_inputs))
    }

    pub(crate) async fn check_local_dep_file_cache(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        declared_outputs: &[BuildArtifact],
    ) -> buck2_error::Result<Option<(ActionOutputs, ActionExecutionMetadata)>> {
        let matching_result = span_async_simple(
            buck2_data::MatchDepFilesStart {
                checking_filtered_inputs: true,
                remote_cache: false,
            },
            match_or_clear_dep_file(
                ctx,
                &self.dep_files_key,
                &self.input_directory_digest,
                &self.common_digests.commandline_cli_digest,
                &self.shared_declared_inputs,
                declared_outputs,
                &self.declared_dep_files,
            ),
            buck2_data::MatchDepFilesEnd {},
        )
        .await?;

        let matching_result = matching_result.map(|o| {
            (
                o,
                ActionExecutionMetadata {
                    execution_kind: ActionExecutionKind::LocalDepFile,
                    timing: Default::default(),
                    input_files_bytes: None,
                },
            )
        });
        Ok(matching_result)
    }

    pub(crate) async fn check_remote_dep_file_entry(
        &self,
        digest_config: DigestConfig,
        fs: &ArtifactFs,
        materializer: &dyn Materializer,
        found: &RemoteDepFile,
    ) -> buck2_error::Result<bool> {
        // Everything in the common digest structure is included in the remote dep file key,
        // so they should be the same but it's good to double check.
        let common = &self.common_digests;
        if common.commandline_cli_digest.as_bytes().to_vec() != found.commandline_cli_digest {
            tracing::debug!("Remote dep files miss: command cli digests are different");
            return Ok(false);
        }
        if common.output_paths_digest.raw_digest().as_bytes().to_vec() != found.output_paths_digest
        {
            tracing::debug!("Remote dep files miss: output paths digest are different");
            return Ok(false);
        }
        if common
            .untagged_inputs_digest
            .raw_digest()
            .as_bytes()
            .to_vec()
            != found.untagged_inputs_digest
        {
            tracing::debug!("Remote dep files miss: untagged inputs digest are different");
            return Ok(false);
        }

        // Ensure the declared dep files are the same
        if self.declared_dep_files.tagged.len() != found.dep_file_inputs.len() {
            tracing::debug!("Remote dep files miss: declared dep file counts are different");
            return Ok(false);
        }
        let different_dep_files_count = self
            .declared_dep_files
            .tagged
            .iter()
            .zip(found.dep_file_inputs.iter())
            .filter(|((_, d1), d2)| d1.output.get_path().to_string() != d2.dep_file_path)
            .count();
        if different_dep_files_count != 0 {
            tracing::debug!("Remote dep files miss: declared dep files are different");
            return Ok(false);
        }

        // Now we compare the filtered input digests.
        // To do this,
        // 1. Read the dep file contents, this should have been downloaded as part of the action cache lookup
        // 2. Filter our inputs so that only the inputs mentioned in the dep files are retained
        // 3. Compare that against the one we retrieved from the remote dep file cache: RemoteDepFile.dep_file_inputs
        let dep_files = read_dep_files(
            false,
            // The declared dep files of this and the found action are the same
            &self.declared_dep_files,
            fs,
            materializer,
        )
        .await
        .buck_error_context("Error reading dep files")?;

        let dep_files = match dep_files {
            Some(dep_files) => dep_files,
            None => {
                tracing::debug!("Remote dep files miss: Dep files cannot be materialized");
                return Ok(false);
            }
        };

        let computed_filtered_fingerprints = self
            .shared_declared_inputs
            .clone()
            .unshare()
            .filter(dep_files)
            .fingerprint(digest_config);

        let different_digest_count = computed_filtered_fingerprints
            .tagged
            .iter()
            .zip(found.dep_file_inputs.iter())
            .filter(|((_, f1), found)| {
                f1.fingerprint().raw_digest().as_bytes().to_vec() != found.filtered_fingerprint
            })
            .count();

        if different_digest_count != 0 {
            tracing::debug!("Remote dep files miss: Filtered input digests are different");
            return Ok(false);
        }

        // Everything matches! The action from cache can be used.
        Ok(true)
    }
}

pub(crate) fn make_dep_file_bundle<'a>(
    ctx: &mut dyn ActionExecutionCtx,
    visitor: DepFilesCommandLineVisitor<'_>,
    expanded_command_line_digest: ExpandedCommandLineDigest,
    execution_paths: &'a CommandExecutionPaths,
) -> buck2_error::Result<DepFileBundle> {
    let input_directory_digest = execution_paths
        .input_directory()
        .fingerprint()
        .data()
        .dupe();
    let dep_files_key = DepFilesKey::from_action_execution_target(ctx.target());

    let DepFilesCommandLineVisitor {
        inputs: declared_inputs,
        tagged_outputs,
        ..
    } = visitor;

    // Filter out tags with no dep file associated with it
    let tagged_outputs: OrderedMap<ArtifactTag, DeclaredDepFile> = tagged_outputs
        .into_iter()
        .filter_map(|(tag, (label, output))| {
            let output = output?;
            Some((tag, DeclaredDepFile { label, output }))
        })
        .collect();

    let declared_dep_files = DeclaredDepFiles {
        tagged: tagged_outputs,
    };

    let shared_declared_inputs = declared_inputs
        .to_directories(ctx)?
        .share(ctx.digest_config());
    let untagged_inputs_digest = shared_declared_inputs.untagged.fingerprint().dupe();

    // Construct digests needed to construct a remote dep file key and remote dep file entry (if needed)
    let common_digests = CommonDigests {
        commandline_cli_digest: expanded_command_line_digest,
        untagged_inputs_digest,
        output_paths_digest: get_output_path_digest(
            ctx.digest_config(),
            execution_paths.output_paths(),
        ),
    };
    let remote_dep_file_action = common_digests.make_remote_dep_file_action(ctx);

    Ok(DepFileBundle {
        dep_files_key,
        remote_dep_file_action,
        input_directory_digest,
        shared_declared_inputs,
        declared_dep_files,
        common_digests,
        // We will lazily compute this when we need it.
        filtered_input_fingerprints: None,
    })
}

/// See if there is an identical action that matches in the cache.
/// If there is, return the outputs. Otherwise, additionally return a boolean to indicate if the lookup was a miss.
#[instrument(level = "debug", skip(input_directory_digest,cli_digest,ctx), fields(key = %key))]
pub(crate) async fn match_if_identical_action(
    ctx: &dyn ActionExecutionCtx,
    key: &DepFilesKey,
    input_directory_digest: &FileDigest,
    cli_digest: &ExpandedCommandLineDigest,
    declared_outputs: &[BuildArtifact],
    declared_dep_files: &DeclaredDepFiles,
) -> buck2_error::Result<(Option<ActionOutputs>, bool)> {
    let previous_state = match get_dep_files(key) {
        Some(d) => d.dupe(),
        None => return Ok((None, false)),
    };

    let actions_match = check_action(
        key,
        &previous_state,
        input_directory_digest,
        cli_digest,
        declared_outputs,
        declared_dep_files,
    );

    if actions_match == InitialDepFileLookupResult::Hit
        && outputs_match(ctx, &previous_state).await?
    {
        tracing::trace!("Dep files are a hit");
        return Ok((Some(previous_state.result.dupe()), false));
    }

    // Don't remote the key from cache in this case as we did not fully check with the dep file content
    tracing::trace!("Local dep file cache does not have an identical action cached");
    Ok((
        None,
        actions_match == InitialDepFileLookupResult::CheckFilteredInputs,
    ))
}

/// Match the dep file recorded for key, or clear it from the map (if it exists).
#[instrument(level = "debug", skip(input_directory_digest,cli_digest,declared_inputs,ctx), fields(key = %key))]
pub(crate) async fn match_or_clear_dep_file(
    ctx: &dyn ActionExecutionCtx,
    key: &DepFilesKey,
    input_directory_digest: &FileDigest,
    cli_digest: &ExpandedCommandLineDigest,
    declared_inputs: &PartitionedInputs<ActionSharedDirectory>,
    declared_outputs: &[BuildArtifact],
    declared_dep_files: &DeclaredDepFiles,
) -> buck2_error::Result<Option<ActionOutputs>> {
    let previous_state = match get_dep_files(key) {
        Some(d) => d.dupe(),
        None => return Ok(None),
    };

    let dep_files_match = dep_files_match(
        key,
        &previous_state,
        input_directory_digest,
        cli_digest,
        declared_inputs,
        declared_outputs,
        declared_dep_files,
        ctx,
    )
    .await?;
    if dep_files_match && outputs_match(ctx, &previous_state).await? {
        tracing::trace!("Dep files are a hit");
        return Ok(Some(previous_state.result.dupe()));
    }

    tracing::trace!("Dep files are a miss, removing the key from cache");
    DEP_FILES.remove(key);

    Ok(None)
}

#[derive(PartialEq)]
enum InitialDepFileLookupResult {
    Hit,
    Miss,
    CheckFilteredInputs,
}

async fn outputs_match(
    ctx: &dyn ActionExecutionCtx,
    previous_state: &Arc<DepFileState>,
) -> buck2_error::Result<bool> {
    let fs = ctx.fs();

    // Finally, we need to make sure that the artifacts in the materializer actually
    // match. This is necessary in case a different action wrote to those artifacts and
    // didn't use the same cache key.
    let output_matches = previous_state
        .result
        .iter()
        .map(|(path, value)| (fs.buck_out_path_resolver().resolve_gen(path), value.dupe()))
        .collect();

    let materializer_accepts = ctx
        .materializer()
        .declare_match(output_matches)
        .await?
        .is_match();
    Ok(materializer_accepts)
}

fn check_action(
    key: &DepFilesKey,
    previous_state: &DepFileState,
    input_directory_digest: &FileDigest,
    cli_digest: &ExpandedCommandLineDigest,
    declared_outputs: &[BuildArtifact],
    declared_dep_files: &DeclaredDepFiles,
) -> InitialDepFileLookupResult {
    if !declared_dep_files.declares_same_dep_files(&previous_state.declared_dep_files) {
        // We first need to check if the same dep files existed before or not. If not, then we
        // can't assume they'll still be on disk, and we have to bail.
        tracing::trace!("Dep files miss: Dep files declaration has changed");
        DEP_FILES.remove(key);
        return InitialDepFileLookupResult::Miss;
    }

    if !outputs_are_reusable(declared_outputs, &previous_state.result) {
        tracing::trace!("Dep files miss: Output declaration has changed");
        DEP_FILES.remove(key);
        return InitialDepFileLookupResult::Miss;
    }

    if *cli_digest != previous_state.digests.cli {
        tracing::trace!("Dep files miss: Command line has changed");
        DEP_FILES.remove(key);
        return InitialDepFileLookupResult::Miss;
    }

    if *input_directory_digest == previous_state.digests.directory {
        // The actions are identical
        tracing::trace!("Dep files hit: Command line and directory have not changed");
        return InitialDepFileLookupResult::Hit;
    }
    InitialDepFileLookupResult::CheckFilteredInputs
}

async fn dep_files_match(
    key: &DepFilesKey,
    previous_state: &DepFileState,
    input_directory_digest: &FileDigest,
    cli_digest: &ExpandedCommandLineDigest,
    declared_inputs: &PartitionedInputs<ActionSharedDirectory>,
    declared_outputs: &[BuildArtifact],
    declared_dep_files: &DeclaredDepFiles,
    ctx: &dyn ActionExecutionCtx,
) -> buck2_error::Result<bool> {
    let initial_check = check_action(
        key,
        previous_state,
        input_directory_digest,
        cli_digest,
        declared_outputs,
        declared_dep_files,
    );
    if initial_check == InitialDepFileLookupResult::Hit {
        return Ok(true);
    }

    // We didn't get an exact match, and we don't have any dep files, so we're done.
    if declared_dep_files.tagged.is_empty() {
        return Ok(false);
    }

    let dep_files = read_dep_files(
        previous_state.has_signatures(),
        previous_state.declared_dep_files(),
        ctx.fs(),
        ctx.materializer(),
    )
    .await
    .buck_error_context(
        "Error reading persisted dep files. \
            Fix the command that produced an invalid dep file. \
            You may also use `buck2 debug flush-dep-files` to drop all dep file state.",
    )?;

    let dep_files = match dep_files {
        Some(dep_files) => dep_files,
        None => {
            tracing::trace!("Dep files miss: Dep files cannot be materialized");
            return Ok(false);
        }
    };

    // Now we need to know the fingerprints on the original action. Produce them if they're
    // missing. We're either storing input directories or outputs here.

    let digest_config = ctx.digest_config();

    let fingerprints_match = {
        // NOTE: We don't bother releasing the guard here (we'd have to clone the fingerprints to do
        // so), because this Mutex won't be contended: only one action will look at its value.
        let previous_fingerprints = previous_state.locked_compute_fingerprints(
            Cow::Borrowed(&dep_files),
            keep_directories()?,
            digest_config,
        );

        // NOTE: We use the new directory to e.g. resolve symlinks referenced in the dep file. This
        // makes sense: if a path in the depfile is still a symlink, then we'll compare the new
        // destination and the old. If it's not, then we can assume that the tool wouldn't traverse
        // the symlink anymore.
        let new_fingerprints = declared_inputs
            .clone()
            .unshare()
            .filter(dep_files)
            .fingerprint(digest_config);
        *previous_fingerprints == new_fingerprints
    };

    Ok(fingerprints_match)
}

/// If an action is unchanged but now requires a different set of outputs, that's not a cache hit
/// because we need to rehash the outputs. Having to re-run the action isn't the best, but it's
/// probably infrequent enough that we seem unlikely to care.
fn outputs_are_reusable(declared_outputs: &[BuildArtifact], outputs: &ActionOutputs) -> bool {
    for out in declared_outputs {
        if outputs.get(out.get_path()).is_none() {
            return false;
        }
    }
    true
}

/// Read the dep files for this DepFileState. This will return None if the dep files cannot be
/// materialized (because they e.g. expired).
pub(crate) async fn read_dep_files(
    has_signatures: bool,
    declared_dep_files: &DeclaredDepFiles,
    fs: &ArtifactFs,
    materializer: &dyn Materializer,
) -> buck2_error::Result<Option<ConcreteDepFiles>> {
    // NOTE: We only materialize if we haven't computed our signatures yet, since we know we
    // can't have computed our signatures without having read the dep file already. In an ideal
    // world this wouldn't be necessary, but in practice contention on the materializer makes
    // this slower.
    if !has_signatures {
        match declared_dep_files.materialize(fs, materializer).await {
            Ok(()) => {}
            Err(MaterializeDepFilesError::NotFound) => return Ok(None),
            Err(e) => return Err(e.into()),
        };
    }

    let dep_files = declared_dep_files.read(fs).buck_error_context(
        "Error reading dep files, verify that the action produced valid output",
    )?;

    Ok(dep_files)
}

fn compute_fingerprints(
    directories: PartitionedInputs<ActionDirectoryBuilder>,
    dep_files: ConcreteDepFiles,
    digest_config: DigestConfig,
    keep_directories: bool,
) -> StoredFingerprints {
    let filtered_directories = directories.filter(dep_files).fingerprint(digest_config);
    if keep_directories {
        StoredFingerprints::Dirs(filtered_directories)
    } else {
        StoredFingerprints::Digests(filtered_directories.as_fingerprints())
    }
}

async fn eagerly_compute_fingerprints(
    digest_config: DigestConfig,
    artifact_fs: &ArtifactFs,
    materializer: &dyn Materializer,
    shared_declared_inputs: &PartitionedInputs<ActionSharedDirectory>,
    declared_dep_files: &DeclaredDepFiles,
) -> buck2_error::Result<StoredFingerprints> {
    let dep_files = read_dep_files(false, declared_dep_files, artifact_fs, materializer)
        .await?
        .buck_error_context("Dep file not found")?;

    let fingerprints = compute_fingerprints(
        shared_declared_inputs.clone().unshare(),
        dep_files,
        digest_config,
        keep_directories()?,
    );
    Ok(fingerprints)
}

/// Post-process the dep files produced by an action.
pub(crate) async fn populate_dep_files(
    ctx: &dyn ActionExecutionCtx,
    dep_file_bundle: DepFileBundle,
    result: &ActionOutputs,
    was_produced_locally: bool,
) -> buck2_error::Result<()> {
    let DepFileBundle {
        declared_dep_files,
        dep_files_key,
        input_directory_digest,
        shared_declared_inputs,
        filtered_input_fingerprints,
        common_digests,
        ..
    } = dep_file_bundle;
    let should_compute_fingerprints =
        declared_dep_files.is_empty() || ctx.run_action_knobs().eager_dep_files;
    let digests = CommandDigests {
        cli: common_digests.commandline_cli_digest,
        directory: input_directory_digest,
    };

    let state = match filtered_input_fingerprints {
        Some(fingerprints) => fingerprints.to_dep_file_state(
            digests,
            declared_dep_files,
            result,
            was_produced_locally,
        ),
        None if should_compute_fingerprints => {
            let fingerprints = eagerly_compute_fingerprints(
                ctx.digest_config(),
                ctx.fs(),
                ctx.materializer(),
                &shared_declared_inputs,
                &declared_dep_files,
            )
            .await?;
            fingerprints.to_dep_file_state(
                digests,
                declared_dep_files,
                result,
                was_produced_locally,
            )
        }
        None => DepFileState {
            digests,
            input_signatures: Mutex::new(DepFileStateInputSignatures::Deferred(Some(
                shared_declared_inputs,
            ))),
            declared_dep_files,
            result: result.dupe(),
            was_produced_locally,
        },
    };

    DEP_FILES.insert(dep_files_key, Arc::new(state));
    Ok(())
}

/// Inputs partitioned by tag. `D` is the representation of the set of inputs.
#[derive(Clone, PartialEq, Eq, Allocative)]
pub(crate) struct PartitionedInputs<D> {
    pub(crate) untagged: D,
    pub(crate) tagged: OrderedMap<Arc<str>, D>,
}

impl<D> PartitionedInputs<D> {
    pub(crate) fn iter(&self) -> impl Iterator<Item = &D> {
        std::iter::once(&self.untagged).chain(self.tagged.values())
    }
}

impl Default for PartitionedInputs<Vec<ArtifactGroup>> {
    fn default() -> Self {
        Self {
            tagged: Default::default(),
            untagged: Default::default(),
        }
    }
}

impl PartitionedInputs<Vec<ArtifactGroup>> {
    /// Add an input to this set of PartitionedInputs.
    fn visit_input(
        &mut self,
        input: ArtifactGroup,
        tag: Option<&ArtifactTag>,
        dep_files: &RunActionDepFiles,
    ) {
        let input_group = match tag {
            None => &mut self.untagged,
            Some(tag) => {
                // NOTE: If an input has a tag that doesn't match a dep file, we don't care about
                // it.
                match dep_files.labels.get(tag) {
                    None => &mut self.untagged,
                    // The tagged inputs have prepopulated keys on creation to ensure sorted keys, so the label must exist.
                    Some(label) => self.tagged.get_mut(label).unwrap(),
                }
            }
        };

        input_group.push(input);
    }

    /// Produce Directories from this set of PartitionedInputs. One directory will be produced for
    /// each tag (and one for untagged). This will actually allocate directories (whereas until now
    /// we only held references to artifacts).
    fn to_directories(
        &self,
        ctx: &dyn ActionExecutionCtx,
    ) -> buck2_error::Result<PartitionedInputs<ActionDirectoryBuilder>> {
        let reduce = |inputs: &[ArtifactGroup]| {
            let mut builder = ActionDirectoryBuilder::empty();

            for input in inputs {
                let input = ctx.artifact_values(input);
                input.add_to_directory(&mut builder, ctx.fs())?;
            }

            buck2_error::Ok(builder)
        };

        Ok(PartitionedInputs {
            untagged: reduce(&self.untagged)?,
            tagged: self
                .tagged
                .iter()
                .map(|(tag, inputs)| buck2_error::Ok((tag.dupe(), reduce(inputs)?)))
                .collect::<Result<_, _>>()?,
        })
    }
}

impl PartitionedInputs<ActionSharedDirectory> {
    fn unshare(self) -> PartitionedInputs<ActionDirectoryBuilder> {
        PartitionedInputs {
            untagged: self.untagged.into_builder(),
            tagged: self
                .tagged
                .into_iter()
                .map(|(k, v)| (k, v.into_builder()))
                .collect(),
        }
    }
}

impl PartitionedInputs<ActionDirectoryBuilder> {
    fn share(self, digest_config: DigestConfig) -> PartitionedInputs<ActionSharedDirectory> {
        PartitionedInputs {
            untagged: self
                .untagged
                .fingerprint(digest_config.as_directory_serializer())
                .shared(&*INTERNER),
            tagged: self
                .tagged
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        v.fingerprint(digest_config.as_directory_serializer())
                            .shared(&*INTERNER),
                    )
                })
                .collect(),
        }
    }

    /// Filter this set of PartitionedInputs using dep files that were produced by the command (or
    /// a previous invocation thereof). For each partition of inputs (i.e. untagged, or specific
    /// tags), only the inputs listed in the dep file will be retained.
    fn filter(mut self, mut concrete_dep_files: ConcreteDepFiles) -> Self {
        fn filter(
            label: &Arc<str>,
            builder: &mut ActionDirectoryBuilder,
            concrete_dep_files: &mut ConcreteDepFiles,
        ) {
            let matching_selector = match concrete_dep_files.contents.get_mut(label) {
                Some(s) => s,
                None => return,
            };

            expand_selector_for_dependencies(builder, matching_selector);

            // NOTE: We ignore the filtering if it produces an invalid directory. If we can't
            // filter, that means we're selecting into a leaf, which means one of two things:
            //
            // - We hit an external symlink, and we can't traverse through those.
            // - The directory structure changed and was a dir is now a leaf, in which case we'll
            // select the leaf but when comparing this to the dir it'll conflict.
            let _ignored = matching_selector.filter(builder);
        }

        for (label, dir) in self.tagged.iter_mut() {
            filter(label, dir, &mut concrete_dep_files);
        }

        self
    }

    /// Compute fingerprints for all the PartitionedInputs in here. This lets us do comparisons.
    fn fingerprint(
        self,
        digest_config: DigestConfig,
    ) -> PartitionedInputs<ActionImmutableDirectory> {
        PartitionedInputs {
            untagged: self
                .untagged
                .fingerprint(digest_config.as_directory_serializer()),
            tagged: self
                .tagged
                .into_iter()
                .map(|(k, v)| (k, v.fingerprint(digest_config.as_directory_serializer())))
                .collect(),
        }
    }
}

impl PartitionedInputs<ActionImmutableDirectory> {
    fn as_fingerprints(&self) -> PartitionedInputs<TrackedFileDigest> {
        PartitionedInputs {
            untagged: self.untagged.fingerprint().dupe(),
            tagged: self
                .tagged
                .iter()
                .map(|(k, v)| (k.dupe(), v.fingerprint().dupe()))
                .collect(),
        }
    }
}

/// A dep file declared by a command. This includes its label (which we use for stability across
/// command executions), as well as the artifact that the command will write the dep file to.
#[derive(Hash, PartialEq, Eq, Debug, Dupe, Clone, Allocative)]
struct DeclaredDepFile {
    label: Arc<str>,
    output: Artifact,
}

/// All the dep files declared by a command;
#[derive(Default, Debug, Allocative)]
pub(crate) struct DeclaredDepFiles {
    tagged: OrderedMap<ArtifactTag, DeclaredDepFile>,
}

impl DeclaredDepFiles {
    fn is_empty(&self) -> bool {
        self.tagged.is_empty()
    }

    /// Given an ActionOutputs, materialize this set of dep files, so that we may read them later.
    async fn materialize(
        &self,
        fs: &ArtifactFs,
        materializer: &dyn Materializer,
    ) -> Result<(), MaterializeDepFilesError> {
        let mut paths = Vec::with_capacity(self.tagged.len());

        for declared_dep_file in self.tagged.values() {
            let dep_file = &declared_dep_file.output;
            let path = dep_file.resolve_path(fs).map_err(|e| {
                MaterializeDepFilesError::MaterializationFailed { source: e.into() }
            })?;
            paths.push(path);
        }

        if paths.is_empty() {
            return Ok(());
        }

        let mut has_not_found = false;
        let mut stream = materializer
            .materialize_many(paths)
            .await
            .map_err(|e| MaterializeDepFilesError::MaterializationFailed { source: e.into() })?;

        while let Some(dep_file) = stream.next().await {
            match dep_file {
                Ok(()) => {}
                Err(MaterializationError::NotFound { .. }) => {
                    has_not_found = true;
                }
                Err(e) => {
                    return Err(MaterializeDepFilesError::MaterializationFailed {
                        source: e.into(),
                    });
                }
            }
        }

        if has_not_found {
            Err(MaterializeDepFilesError::NotFound)
        } else {
            Ok(())
        }
    }

    /// Read this set of dep files, producing ConcreteDepFiles. This can then be used to compute
    /// signatures for the input set that was used, and for future input sets.
    fn read(&self, fs: &ArtifactFs) -> buck2_error::Result<Option<ConcreteDepFiles>> {
        let mut contents = HashMap::with_capacity(self.tagged.len());

        for declared_dep_file in self.tagged.values() {
            let mut selector = DirectorySelector::empty();

            let dep_file = declared_dep_file.output.resolve_path(fs)?;

            let read_dep_file: buck2_error::Result<()> = try {
                let dep_file_path = fs.fs().resolve(&dep_file);
                let dep_file = fs_util::read_to_string_if_exists(&dep_file_path)?;

                let dep_file = match dep_file {
                    Some(dep_file) => dep_file,
                    None => {
                        soft_error!(
                            "missing_dep_file",
                            buck2_error::buck2_error!(
                                buck2_error::ErrorTag::Input,
                                "Dep file is missing at {}",
                                dep_file_path
                            )
                            .into()
                        )?;
                        return Ok(None);
                    }
                };

                for line in dep_file.split('\n') {
                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }

                    // On windows, valid dep files can contain backslashes in paths, normalize them.
                    let path = ForwardRelativePathNormalizer::normalize_path(line)
                        .buck_error_context("Invalid line encountered in dep file")?;
                    selector.select(path.as_ref());
                }
            };

            read_dep_file.with_buck_error_context(|| {
                format!(
                    "Action execution produced an invalid `{}` dep file at `{}`",
                    declared_dep_file.label, dep_file,
                )
            })?;

            contents.insert(declared_dep_file.label.dupe(), selector);
        }

        Ok(Some(ConcreteDepFiles { contents }))
    }

    /// Returns whether two DeclaredDepFile instances have the same dep files. This ignores the tag
    /// identity, but it requires the same paths declared using the same name. This is a
    /// pre-requisite for being able to reuse dep files from a previous invocation.
    fn declares_same_dep_files(&self, other: &Self) -> bool {
        let this = self.tagged.values().collect::<HashSet<_>>();
        let other = other.tagged.values().collect::<HashSet<_>>();
        this == other
    }
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
enum MaterializeDepFilesError {
    #[error("Error materializing dep file")]
    MaterializationFailed {
        #[source]
        source: buck2_error::Error,
    },

    #[error("A dep file was not found")]
    NotFound,
}

/// A set of concrete dep files. That is, given a label, a selector that represents the subset of
/// files whose tags matches this label that should be considered relevant.
#[derive(Clone)]
pub(crate) struct ConcreteDepFiles {
    contents: HashMap<Arc<str>, DirectorySelector>,
}

/// A command line visitor to collect inputs and outputs in a form relevant for dep files
/// computations.
pub(crate) struct DepFilesCommandLineVisitor<'a> {
    pub(crate) inputs: PartitionedInputs<Vec<ArtifactGroup>>,
    pub(crate) tagged_outputs: OrderedMap<ArtifactTag, (Arc<str>, Option<Artifact>)>,
    dep_files: &'a RunActionDepFiles,
}

impl<'a> DepFilesCommandLineVisitor<'a> {
    pub(crate) fn new(dep_files: &'a RunActionDepFiles) -> Self {
        // Prepopulate inputs & outputs to maintain the ordering of the labels declaration.

        let tagged_inputs: OrderedMap<Arc<str>, Vec<ArtifactGroup>> = dep_files
            .labels
            .iter()
            .map(|(_tag, label)| (label.dupe(), Default::default()))
            .collect();

        let tagged_outputs: OrderedMap<ArtifactTag, (Arc<str>, Option<Artifact>)> = dep_files
            .labels
            .iter()
            .map(|(tag, label)| (tag.dupe(), (label.dupe(), None)))
            .collect();
        Self {
            inputs: PartitionedInputs {
                untagged: Default::default(),
                tagged: tagged_inputs,
            },
            tagged_outputs,
            dep_files,
        }
    }
}

impl CommandLineArtifactVisitor for DepFilesCommandLineVisitor<'_> {
    fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>) {
        self.inputs.visit_input(input, tag, self.dep_files);
    }

    fn visit_output(&mut self, artifact: OutputArtifact, tag: Option<&ArtifactTag>) {
        match tag {
            Some(tag) => {
                // NOTE: We have validated tags earlier, so we know that if a tag does not point to
                // a dep file here, it's safe to ignore it. We also know that we'll have exactly 1
                // dep file per tag.
                if let Some((_label, output)) = self.tagged_outputs.get_mut(tag) {
                    // NOTE: analysis has been done so we know inputs are bound now.
                    *output = Some((*artifact).dupe().ensure_bound().unwrap().into_artifact());
                }
            }
            None => (),
        }
    }
}

#[cfg(test)]
mod tests {

    use buck2_artifact::actions::key::ActionIndex;
    use buck2_artifact::artifact::artifact_type::testing::BuildArtifactTestingExt;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::target::configured_target_label::ConfiguredTargetLabel;

    use super::*;

    #[test]
    fn test_dep_files_visitor_output_collection() {
        let tag1 = ArtifactTag::new();
        let tag2 = ArtifactTag::new();
        let tag3 = ArtifactTag::new();
        let tag4 = ArtifactTag::new();

        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());
        let artifact1 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            "foo/bar1.h",
            ActionIndex::new(0),
        ));
        let artifact2 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            "foo/bar2.h",
            ActionIndex::new(0),
        ));
        let artifact3 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            "foo/bar3.h",
            ActionIndex::new(0),
        ));
        let artifact4 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            "foo/bar4.h",
            ActionIndex::new(0),
        ));
        let artifact5 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            "foo/bar5.h",
            ActionIndex::new(0),
        ));

        let dep_files = RunActionDepFiles {
            labels: OrderedMap::from_iter([
                (tag1.dupe(), Arc::from("l1")),
                (tag2.dupe(), Arc::from("l2")),
                (tag3.dupe(), Arc::from("l3")),
            ]),
        };

        let mut visitor = DepFilesCommandLineVisitor::new(&dep_files);
        visitor.visit_output(artifact3.as_output_artifact().unwrap(), Some(&tag3));
        visitor.visit_output(artifact2.as_output_artifact().unwrap(), Some(&tag2));
        visitor.visit_output(artifact1.as_output_artifact().unwrap(), Some(&tag1));
        // This should be ignored as it's not included in RunActionDepFiles
        visitor.visit_output(artifact4.as_output_artifact().unwrap(), Some(&tag4));
        // This should be ignored as it does not have a tag
        visitor.visit_output(artifact5.as_output_artifact().unwrap(), None);

        // Assert that the order is preserved between the two maps
        let x: Vec<_> = visitor.tagged_outputs.keys().collect();
        let y: Vec<_> = dep_files.labels.keys().collect();
        assert_eq!(x, y);
    }

    #[test]
    fn test_declares_same_dep_files() {
        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());

        let artifact1 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            "foo/bar1.h",
            ActionIndex::new(0),
        ));

        let artifact2 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            "foo/bar2.h",
            ActionIndex::new(0),
        ));

        let depfile1 = DeclaredDepFile {
            label: Arc::from("foo"),
            output: artifact1,
        };

        let depfile2 = DeclaredDepFile {
            label: Arc::from("foo"),
            output: artifact2.dupe(),
        };

        let depfile3 = DeclaredDepFile {
            label: Arc::from("bar"),
            output: artifact2,
        };

        let tag1 = ArtifactTag::new();

        let tag2 = ArtifactTag::new();

        let decl1 = DeclaredDepFiles {
            tagged: OrderedMap::from_iter([(tag1.dupe(), depfile1.dupe())]),
        };

        let decl2 = DeclaredDepFiles {
            tagged: OrderedMap::from_iter([(tag2.dupe(), depfile1.dupe())]),
        };

        let decl3 = DeclaredDepFiles {
            tagged: OrderedMap::from_iter([(tag2.dupe(), depfile2.dupe())]),
        };

        let decl4 = DeclaredDepFiles {
            tagged: OrderedMap::from_iter([(tag2.dupe(), depfile3.dupe())]),
        };

        assert!(decl1.declares_same_dep_files(&decl1));
        assert!(decl1.declares_same_dep_files(&decl2));
        assert!(!decl2.declares_same_dep_files(&decl3));
        assert!(!decl3.declares_same_dep_files(&decl4));
    }
}
