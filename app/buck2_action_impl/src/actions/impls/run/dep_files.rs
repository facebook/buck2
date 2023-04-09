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
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use buck2_build_api::actions::artifact::artifact_type::Artifact;
use buck2_build_api::actions::artifact::artifact_type::OutputArtifact;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_execution_target::ActionExecutionTarget;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::impls::dep_files::FLUSH_DEP_FILES;
use buck2_build_api::actions::impls::expanded_command_line::ExpandedCommandLineDigest;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::category::Category;
use buck2_core::directory::DirectorySelector;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::fs_util;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::soft_error;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::expand_selector_for_dependencies;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionImmutableDirectory;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::directory::INTERNER;
use buck2_execute::materialize::materializer::MaterializationError;
use buck2_execute::materialize::materializer::Materializer;
use ctor::ctor;
use dashmap::DashMap;
use derive_more::Display;
use dupe::Dupe;
use futures::StreamExt;
use once_cell::sync::Lazy;
use parking_lot::MappedMutexGuard;
use parking_lot::Mutex;
use parking_lot::MutexGuard;
use thiserror::Error;
use tracing::instrument;

#[allocative::root]
static DEP_FILES: Lazy<DashMap<DepFilesKey, Arc<DepFileState>>> = Lazy::new(DashMap::new);

/// When this is set, we retain directories after fingerprintig, so that we can output them later
/// for debugging via `buck2 audit dep-files`.
static KEEP_DIRECTORIES: EnvHelper<bool> = EnvHelper::new("BUCK2_KEEP_DEP_FILE_DIRECTORIES");

/// Forget about all dep files. This isn't really meant to be commonly used, but if an invalid dep
/// file was produced and the user wants unblocking, this will provide it.
fn flush_dep_files() {
    DEP_FILES.clear();
}

#[ctor]
fn set_flush_dep_files() {
    FLUSH_DEP_FILES.init(flush_dep_files);
}

pub fn get_dep_files(key: &DepFilesKey) -> Option<Arc<DepFileState>> {
    DEP_FILES.get(key).map(|s| s.dupe())
}

/// A key used to associate a RunAction with a possible previous dep file.
#[derive(Eq, PartialEq, Hash, Display, Allocative)]
#[display(
    fmt = "{} {} {}",
    owner,
    category,
    "identifier.as_deref().unwrap_or(\"<no identifier>\")"
)]
pub struct DepFilesKey {
    owner: BaseDeferredKey,
    category: Category,
    identifier: Option<String>,
}

impl DepFilesKey {
    pub fn new(owner: BaseDeferredKey, category: Category, identifier: Option<String>) -> Self {
        Self {
            owner,
            category,
            identifier,
        }
    }

    pub fn from_action_execution_target(target: ActionExecutionTarget<'_>) -> Self {
        Self {
            owner: target.owner().dupe(),
            category: target.category().clone(),
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

#[derive(Allocative)]
pub enum StoredFingerprints {
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
pub struct DepFileState {
    digests: CommandDigests,
    input_signatures: Mutex<DepFileStateInputSignatures>,
    declared_dep_files: DeclaredDepFiles,
    result: ActionOutputs,
}

#[derive(Allocative)]
pub struct CommandDigests {
    pub cli: ExpandedCommandLineDigest,
    pub directory: FileDigest,
}

impl DepFileState {
    /// Read the dep files for this DepFileState. This will return None if the dep files cannot be
    /// materialized (because they e.g. expired).
    pub async fn read_dep_files(
        &self,
        fs: &ArtifactFs,
        materializer: &dyn Materializer,
    ) -> anyhow::Result<Option<ConcreteDepFiles>> {
        // NOTE: We only materialize if we haven't computed our signatures yet, since we know we
        // can't have computed our signatures without having read the dep file already. In an ideal
        // world this wouldn't be necessary, but in practice contention on the materializer makes
        // this slower.
        if !self.has_signatures() {
            match self.declared_dep_files.materialize(fs, materializer).await {
                Ok(()) => {}
                Err(MaterializeDepFilesError::NotFound) => return Ok(None),
                Err(e) => return Err(e.into()),
            };
        }

        let dep_files = self
            .declared_dep_files
            .read(fs)
            .context("Error reading dep files, verify that the action produced valid output")?;

        Ok(dep_files)
    }

    fn has_signatures(&self) -> bool {
        match *self.input_signatures.lock() {
            DepFileStateInputSignatures::Computed(..) => true,
            DepFileStateInputSignatures::Deferred(..) => false,
        }
    }

    /// Compute the signature for this DepFileState, having provided the dep files from
    /// read_dep_files.
    pub fn locked_compute_fingerprints<'a>(
        &'a self,
        dep_files: Cow<'_, ConcreteDepFiles>,
        keep_directories: bool,
        digest_config: DigestConfig,
    ) -> MappedMutexGuard<'a, StoredFingerprints> {
        // Now we need to know the signatures on the original action. Produce them if they're
        // missing. We're either storing input directories or outputs here.

        let mut guard = self.input_signatures.lock();

        if let DepFileStateInputSignatures::Deferred(ref mut directories) = *guard {
            let directories = directories
                .take()
                .expect("Poisoned DepFileStateInputSignatures")
                .unshare()
                .filter(dep_files.into_owned())
                .fingerprint(digest_config);

            let fingerprints = if keep_directories {
                StoredFingerprints::Dirs(directories)
            } else {
                StoredFingerprints::Digests(directories.as_fingerprints())
            };

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
    pub(crate) labels: HashMap<ArtifactTag, Arc<str>>,
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
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
        }
    }
}

/// Match the dep file recorded for key, or clear it from the map (if it exists).
#[instrument(level = "debug", skip(digests, declared_inputs, ctx), fields(key = %key))]
pub(crate) async fn match_or_clear_dep_file(
    key: &DepFilesKey,
    digests: &CommandDigests,
    declared_inputs: &PartitionedInputs<Vec<ArtifactGroup>>,
    declared_outputs: &[BuildArtifact],
    declared_dep_files: &DeclaredDepFiles,
    ctx: &dyn ActionExecutionCtx,
) -> anyhow::Result<Option<ActionOutputs>> {
    let previous_state = match get_dep_files(key) {
        Some(d) => d.dupe(),
        None => return Ok(None),
    };

    if dep_files_match(
        &previous_state,
        digests,
        declared_inputs,
        declared_outputs,
        declared_dep_files,
        ctx,
    )
    .await?
    {
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

        if materializer_accepts {
            tracing::trace!("Dep files are a hit");
            return Ok(Some(previous_state.result.dupe()));
        }
    }

    tracing::trace!("Dep files are a miss");
    DEP_FILES.remove(key);
    Ok(None)
}

async fn dep_files_match(
    previous_state: &DepFileState,
    digests: &CommandDigests,
    declared_inputs: &PartitionedInputs<Vec<ArtifactGroup>>,
    declared_outputs: &[BuildArtifact],
    declared_dep_files: &DeclaredDepFiles,
    ctx: &dyn ActionExecutionCtx,
) -> anyhow::Result<bool> {
    if !declared_dep_files.declares_same_dep_files(&previous_state.declared_dep_files) {
        // We first need to check if the same dep files existed before or not. If not, then we
        // can't assume they'll still be on disk, and we have to bail.
        tracing::trace!("Dep files miss: Dep files declaration has changed");
        return Ok(false);
    }

    if !outputs_are_reusable(declared_outputs, &previous_state.result) {
        tracing::trace!("Dep files miss: Output declaration has changed");
        return Ok(false);
    }

    if digests.cli != previous_state.digests.cli {
        tracing::trace!("Dep files miss: Command line has changed");
        return Ok(false);
    }

    if digests.directory == previous_state.digests.directory {
        tracing::trace!("Dep files hit: Command line and directory have not changed");
        return Ok(true);
    }

    let dep_files = previous_state
        .read_dep_files(ctx.fs(), ctx.materializer())
        .await
        .context(
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
            KEEP_DIRECTORIES.get_copied()?.unwrap_or_default(),
            digest_config,
        );

        // NOTE: We use the new directory to e.g. resolve symlinks referenced in the dep file. This
        // makes sense: if a path in the depfile is still a symlink, then we'll compare the new
        // destination and the old. If it's not, then we can assume that the tool wouldn't traverse
        // the symlink anymore.
        let new_fingerprints = declared_inputs
            .to_directories(ctx)?
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

/// Post-process the dep files produced by an action.
pub(crate) async fn populate_dep_files(
    key: DepFilesKey,
    digests: CommandDigests,
    declared_inputs: PartitionedInputs<Vec<ArtifactGroup>>,
    declared_dep_files: DeclaredDepFiles,
    result: &ActionOutputs,
    ctx: &dyn ActionExecutionCtx,
) -> anyhow::Result<()> {
    let has_no_dep_files = declared_dep_files.is_empty();

    let directories = declared_inputs.to_directories(ctx)?;
    let digest_config = ctx.digest_config();

    let state = DepFileState {
        digests,
        input_signatures: Mutex::new(DepFileStateInputSignatures::Deferred(Some(
            directories.share(digest_config),
        ))),
        declared_dep_files,
        result: result.dupe(),
    };

    if has_no_dep_files || ctx.run_action_knobs().eager_dep_files {
        let dep_files = state
            .read_dep_files(ctx.fs(), ctx.materializer())
            .await?
            .context("Dep file not found")?;

        // Evaluate the fingerprints, but release the lock immediately.
        std::mem::drop(state.locked_compute_fingerprints(
            Cow::Owned(dep_files),
            KEEP_DIRECTORIES.get_copied()?.unwrap_or_default(),
            digest_config,
        ));
    }

    DEP_FILES.insert(key, Arc::new(state));

    Ok(())
}

/// Inputs partitioned by tag. `D` is the representation of the set of inputs.
#[derive(Clone, PartialEq, Eq, Allocative)]
pub struct PartitionedInputs<D> {
    pub untagged: D,
    pub tagged: HashMap<Arc<str>, D>,
}

impl<D> PartitionedInputs<D> {
    pub fn iter(&self) -> impl Iterator<Item = &D> {
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
                    Some(label) => self.tagged.entry(label.dupe()).or_default(),
                    None => &mut self.untagged,
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
    ) -> anyhow::Result<PartitionedInputs<ActionDirectoryBuilder>> {
        let reduce = |inputs: &[ArtifactGroup]| {
            let mut builder = ActionDirectoryBuilder::empty();

            for input in inputs {
                let input = ctx.artifact_values(input);
                input.add_to_directory(&mut builder, ctx.fs())?;
            }

            anyhow::Ok(builder)
        };

        Ok(PartitionedInputs {
            untagged: reduce(&self.untagged)?,
            tagged: self
                .tagged
                .iter()
                .map(|(tag, inputs)| anyhow::Ok((tag.dupe(), reduce(inputs)?)))
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
    tagged: HashMap<ArtifactTag, DeclaredDepFile>,
}

impl DeclaredDepFiles {
    fn is_empty(&self) -> bool {
        self.tagged.is_empty()
    }

    /// Add dep file to this set.
    fn visit_output(
        &mut self,
        artifact: OutputArtifact,
        tag: Option<&ArtifactTag>,
        dep_files: &RunActionDepFiles,
    ) {
        match tag {
            None => {}
            Some(tag) => {
                // NOTE: We have validated tags earlier, so we know that if a tag does not point to
                // a dep file here, it's safe to ignore it. We also know that we'll have exactly 1
                // dep file per tag.
                if let Some(label) = dep_files.labels.get(tag) {
                    // NOTE: analysis has been done so we know inputs are bound now.
                    let output = (*artifact).dupe().ensure_bound().unwrap().into_artifact();

                    self.tagged.insert(
                        tag.dupe(),
                        DeclaredDepFile {
                            label: label.dupe(),
                            output,
                        },
                    );
                }
            }
        }
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
            let path = dep_file
                .resolve_path(fs)
                .map_err(|e| MaterializeDepFilesError::MaterializationFailed { source: e })?;
            paths.push(path);
        }

        if paths.is_empty() {
            return Ok(());
        }

        let mut has_not_found = false;
        let mut stream = materializer
            .materialize_many(paths)
            .await
            .map_err(|e| MaterializeDepFilesError::MaterializationFailed { source: e })?;

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
    fn read(&self, fs: &ArtifactFs) -> anyhow::Result<Option<ConcreteDepFiles>> {
        let mut contents = HashMap::with_capacity(self.tagged.len());

        for declared_dep_file in self.tagged.values() {
            let mut selector = DirectorySelector::empty();

            let dep_file = declared_dep_file.output.resolve_path(fs)?;

            let read_dep_file: anyhow::Result<()> = try {
                let dep_file_path = fs.fs().resolve(&dep_file);
                let dep_file = fs_util::read_to_string_opt(&dep_file_path)?;

                let dep_file = match dep_file {
                    Some(dep_file) => dep_file,
                    None => {
                        soft_error!(
                            "missing_dep_file",
                            anyhow::anyhow!("Dep file is missing at {}", dep_file_path)
                        )?;
                        return Ok(None);
                    }
                };

                for line in dep_file.split('\n') {
                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }
                    let path = ProjectRelativePath::new(line)
                        .context("Invalid line encountered in dep file")?;

                    selector.select(path);
                }
            };

            read_dep_file.with_context(|| {
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

#[derive(Error, Debug)]
enum MaterializeDepFilesError {
    #[error("Error materializing dep file")]
    MaterializationFailed {
        #[source]
        source: anyhow::Error,
    },

    #[error("A dep file was not found")]
    NotFound,
}

/// A set of concrete dep files. That is, given a label, a selector that represents the subset of
/// files whose tags matches this label that should be considered relevant.
#[derive(Clone)]
pub struct ConcreteDepFiles {
    contents: HashMap<Arc<str>, DirectorySelector>,
}

/// A command line visitor to collect inputs and outputs in a form relevant for dep files
/// computations.
pub(crate) struct DepFilesCommandLineVisitor<'a> {
    pub inputs: PartitionedInputs<Vec<ArtifactGroup>>,
    pub outputs: DeclaredDepFiles,
    dep_files: &'a RunActionDepFiles,
}

impl<'a> DepFilesCommandLineVisitor<'a> {
    pub(crate) fn new(dep_files: &'a RunActionDepFiles) -> Self {
        Self {
            inputs: Default::default(),
            outputs: Default::default(),
            dep_files,
        }
    }
}

impl CommandLineArtifactVisitor for DepFilesCommandLineVisitor<'_> {
    fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>) {
        self.inputs.visit_input(input, tag, self.dep_files);
    }

    fn visit_output(&mut self, artifact: OutputArtifact, tag: Option<&ArtifactTag>) {
        self.outputs.visit_output(artifact, tag, self.dep_files);
    }
}

#[cfg(test)]
mod test {
    use buck2_build_api::actions::artifact::artifact_type::testing::BuildArtifactTestingExt;
    use buck2_build_api::deferred::types::testing::DeferredIdExt;
    use buck2_build_api::deferred::types::DeferredId;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::target::label::ConfiguredTargetLabel;
    use maplit::hashmap;

    use super::*;

    #[test]
    fn test_declares_same_dep_files() {
        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());

        let artifact1 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            ForwardRelativePathBuf::unchecked_new("foo/bar1.h".to_owned()),
            DeferredId::testing_new(0),
        ));

        let artifact2 = Artifact::from(BuildArtifact::testing_new(
            target.dupe(),
            ForwardRelativePathBuf::unchecked_new("foo/bar2.h".to_owned()),
            DeferredId::testing_new(0),
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
            tagged: hashmap! { tag1.dupe() => depfile1.dupe() },
        };

        let decl2 = DeclaredDepFiles {
            tagged: hashmap! { tag2.dupe() => depfile1.dupe() },
        };

        let decl3 = DeclaredDepFiles {
            tagged: hashmap! { tag2.dupe() => depfile2.dupe() },
        };

        let decl4 = DeclaredDepFiles {
            tagged: hashmap! { tag2.dupe() => depfile3.dupe() },
        };

        assert!(decl1.declares_same_dep_files(&decl1));
        assert!(decl1.declares_same_dep_files(&decl2));
        assert!(!decl2.declares_same_dep_files(&decl3));
        assert!(!decl3.declares_same_dep_files(&decl4));
    }
}
