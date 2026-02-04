/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::iter::zip;
use std::sync::Arc;

use allocative::Allocative;
use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::ArtifactKind;
use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::file_ops::dice::DiceFileComputations;
use buck2_common::file_ops::metadata::RawPathMetadata;
use buck2_common::file_ops::metadata::RawSymlink;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::package_listing::dice::DicePackageListingResolver;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::PackageLabel;
use buck2_directory::directory::directory_data::DirectoryData;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::directory::INTERNER;
use buck2_execute::directory::extract_artifact_value;
use buck2_execute::directory::insert_artifact;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::Future;
use futures::FutureExt;
use itertools::Itertools;
use ref_cast::RefCast;
use smallvec::SmallVec;
use sorted_vector_map::SortedVectorMap;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::actions::calculation::ActionCalculation;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::artifact_groups::ResolvedArtifactGroup;
use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::keep_going::KeepGoing;

#[async_trait]
pub trait ArtifactGroupCalculation {
    /// Makes an 'Artifact' available to be accessed
    async fn ensure_artifact_group(
        &mut self,
        input: &ArtifactGroup,
    ) -> buck2_error::Result<ArtifactGroupValues>;
}

#[async_trait]
impl ArtifactGroupCalculation for DiceComputations<'_> {
    /// makes the 'Artifact' available to be accessed
    async fn ensure_artifact_group(
        &mut self,
        input: &ArtifactGroup,
    ) -> buck2_error::Result<ArtifactGroupValues> {
        // TODO consider if we need to cache this
        let resolved_artifacts = input.resolved_artifact(self).await?;
        ensure_artifact_group_staged(self, resolved_artifacts.clone())
            .await?
            .to_group_values(&resolved_artifacts)
    }
}

/// A large build may have many artifact dependency edges and so may have many of the
/// `ensure_build_artifact_*()` futures live at any time. To support this efficiently
/// we provide these `*_staged()` functions that provide an optimized Future implementation
/// for waiting on the dependency edge and an optimized form to represent the result (as
/// we also may have many of those results alive across await points as things need to wait
/// on all dependencies).
///
/// Performance sensitive things should use these staged functions, wait for all their results
/// and then synchronously process them and drop any intermediate data structures before their
/// next yield point.
///
/// Some of the optimizations this provides:
///  - The staged future is kept to a minimum size (which we track in an assertion below).
///  - The result of the staged future is kept to a minimum size (also tracked below).
///  - For the single Artifact case from ensure_artifact_group_staged, we defer allocation
///    of the ArtifactGroupValues until `to_group_values()` is called. For callers waiting
///    on many inputs, this allows them to only allocate those large values only after all
///    inputs are ready.
pub(crate) fn ensure_artifact_group_staged<'a>(
    ctx: &'a mut DiceComputations,
    input: ResolvedArtifactGroup<'a>,
) -> impl Future<Output = buck2_error::Result<EnsureArtifactGroupReady>> + use<'a> {
    match input {
        ResolvedArtifactGroup::Artifact(artifact) => {
            ensure_artifact_staged(ctx, artifact.clone()).left_future()
        }
        ResolvedArtifactGroup::TransitiveSetProjection(key) => ctx
            .compute(EnsureTransitiveSetProjectionKey::ref_cast(key))
            .map(|v| Ok(EnsureArtifactGroupReady::TransitiveSet(v??)))
            .right_future(),
    }
}

/// See [ensure_artifact_group_staged].
pub(super) fn ensure_base_artifact_staged<'a>(
    dice: &'a mut DiceComputations,
    artifact: BaseArtifactKind,
) -> impl Future<Output = buck2_error::Result<EnsureArtifactGroupReady>> + use<'a> {
    match artifact {
        BaseArtifactKind::Build(built) => ensure_build_artifact_staged(dice, built).left_future(),
        BaseArtifactKind::Source(source) => {
            ensure_source_artifact_staged(dice, source).right_future()
        }
    }
}

/// See [ensure_artifact_group_staged].
pub(super) fn ensure_artifact_staged<'a>(
    dice: &'a mut DiceComputations,
    artifact: Artifact,
) -> impl Future<Output = buck2_error::Result<EnsureArtifactGroupReady>> + use<'a> {
    let ArtifactKind { base, path } = artifact.data();
    match path.is_empty() {
        true => ensure_base_artifact_staged(dice, base.clone()).left_future(),
        false => dice
            .compute(EnsureProjectedArtifactKey::ref_cast(artifact.data()))
            .map(|v| Ok(EnsureArtifactGroupReady::Single(v??)))
            .right_future(),
    }
}

fn ensure_build_artifact_staged<'a>(
    dice: &'a mut DiceComputations,
    built: BuildArtifact,
) -> impl Future<Output = buck2_error::Result<EnsureArtifactGroupReady>> + use<'a> {
    ActionCalculation::build_action(dice, built.key()).map(move |action_outputs| {
        let action_outputs = action_outputs?;
        if let Some(value) = action_outputs.get(built.get_path()) {
            Ok(EnsureArtifactGroupReady::Single(value.dupe()))
        } else {
            Err(
                EnsureArtifactStagedError::BuildArtifactMissing(built.clone(), action_outputs)
                    .into(),
            )
        }
    })
}

fn ensure_source_artifact_staged<'a>(
    dice: &'a mut DiceComputations,
    source: SourceArtifact,
) -> impl Future<Output = buck2_error::Result<EnsureArtifactGroupReady>> + use<'a> {
    async move {
        Ok(EnsureArtifactGroupReady::Single(
            path_artifact_value(
                dice,
                Arc::new(source.get_path().to_cell_path()),
                Some(source.get_path().package()),
            )
            .await?,
        ))
    }
    .boxed()
}

// These errors should be unreachable, they indicate misuse of the staged ensure artifact (or other buck
// invariant violations), but it's still better to propagate them as Error than to panic!().
#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub enum EnsureArtifactStagedError {
    #[error("Tried to unpack single artifact, but got transitive set")]
    UnpackSingleTransitiveSet,
    #[error("Expected a transitive set, got a single artifact")]
    ExpectedTransitiveSet,
    // This one could probably be a panic! if DICE didn't eagerly re-evaluate all deps.
    #[error("Building an artifact didn't produce it. Expected `{}` but only have `{}`", .0.get_path(), display_outputs(.1))]
    BuildArtifactMissing(BuildArtifact, ActionOutputs),
}

fn display_outputs(outputs: &ActionOutputs) -> String {
    format!(
        "({})",
        outputs
            .iter()
            .map(|(path, _)| path.path())
            .sorted()
            .join(", ")
    )
}

/// Represents the "ready" stage of an ensure_artifact_*() call. At this point the
/// ArtifactValue/ArtifactGroupValues can be synchronously accessed/constructed.
pub(crate) enum EnsureArtifactGroupReady {
    Single(ArtifactValue),
    TransitiveSet(ArtifactGroupValues),
}

impl EnsureArtifactGroupReady {
    /// Converts the ensured artifact to an ArtifactGroupValues. The caller must ensure that the passed in artifact
    /// is the same one that was used to ensure this.
    pub(crate) fn to_group_values<'v>(
        self,
        resolved_artifact_group: &ResolvedArtifactGroup<'v>,
    ) -> buck2_error::Result<ArtifactGroupValues> {
        match self {
            EnsureArtifactGroupReady::TransitiveSet(values) => Ok(values),
            EnsureArtifactGroupReady::Single(value) => match resolved_artifact_group {
                ResolvedArtifactGroup::Artifact(artifact) => {
                    Ok(ArtifactGroupValues::from_artifact(artifact.clone(), value))
                }
                ResolvedArtifactGroup::TransitiveSetProjection(_) => {
                    Err(EnsureArtifactStagedError::ExpectedTransitiveSet.into())
                }
            },
        }
    }

    fn unpack_single(self) -> buck2_error::Result<ArtifactValue> {
        match self {
            EnsureArtifactGroupReady::Single(value) => Ok(value),
            EnsureArtifactGroupReady::TransitiveSet(..) => {
                Err(EnsureArtifactStagedError::UnpackSingleTransitiveSet.into())
            }
        }
    }
}

static_assertions::assert_eq_size!(EnsureArtifactGroupReady, [usize; 4]);

// This assertion assures we don't unknowingly regress the size of this critical future.
// TODO(cjhopman): We should be able to wrap this in a convenient assertion macro.
#[allow(unused, clippy::diverging_sub_expression)]
fn _assert_ensure_artifact_group_future_size() {
    let mut ctx: DiceComputations = panic!();

    // These first two are the important ones to track and not regress.
    let v = ctx.ensure_artifact_group(panic!());
    let e = [0u8; 128 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);

    let v = ensure_artifact_group_staged(&mut ctx, panic!());
    let e = [0u8; 1088 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);

    // The rest of these are to help understand how changes are impacting the important ones above. Regressing these
    // is generally okay if the above don't regress.
    let v = ensure_artifact_staged(&mut ctx, panic!());
    let e = [0u8; 1088 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);

    let v = ensure_base_artifact_staged(&mut ctx, panic!());
    let e = [0u8; 1088 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);

    let v = ensure_build_artifact_staged(&mut ctx, panic!());
    let e = [0u8; 1088 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);

    let v = ActionCalculation::build_action(&mut ctx, panic!());
    let e = [0u8; 704 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);

    let v = ensure_source_artifact_staged(&mut ctx, panic!());
    let e = [0u8; 128 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);
}

async fn dir_artifact_value(
    ctx: &mut DiceComputations<'_>,
    cell_path: Arc<CellPath>,
) -> buck2_error::Result<ArtifactValue> {
    // We kept running into this performance footgun where a large directory is declared as a source
    // on a toolchain, and then every `BuildKey` using that toolchain ends up taking a DICE edge on
    // `PathMetadataKey` of every file inside that directory, blowing up Buck2's memory use.
    // `DirArtifactValueKey` is an intermediate DICE key to prevent that -  every `BuildKey` using
    // that directory now only depends on one `DirArtifactValueKey`, and that `DirArtifactValueKey`
    // depends on the `PathMetadataKey` of every member of the directory.
    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    #[display("dir_artifact_value({})", _0)]
    struct DirArtifactValueKey(Arc<CellPath>);

    #[async_trait]
    impl Key for DirArtifactValueKey {
        type Value = buck2_error::Result<ArtifactValue>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellation: &CancellationContext,
        ) -> Self::Value {
            let files = DiceFileComputations::read_dir(ctx, self.0.as_ref().as_ref())
                .await?
                .included;

            let entry_values = ctx
                .try_compute_join(files.iter(), |ctx, x| {
                    async move {
                        // TODO(scottcao): This current creates a `DirArtifactValueKey` for each subdir of a source directory.
                        // Instead, this should be 1 key for the entire top-level directory since there's almost
                        // no chance of getting cache hit with a sub-directory.
                        let value = path_artifact_value(
                            ctx,
                            Arc::new(self.0.as_ref().join(&x.file_name)),
                            None,
                        )
                        .await?;
                        buck2_error::Ok((x.file_name.clone(), value))
                    }
                    .boxed()
                })
                .await?;

            enum DepsMerger {
                None,
                One(ActionSharedDirectory),
                Multiple(ActionDirectoryBuilder),
            }

            let mut entries = SortedVectorMap::new();
            let mut deps_merger = DepsMerger::None;
            for (file_name, value) in entry_values {
                entries.insert(file_name, value.entry().dupe());
                if let Some(deps) = value.deps() {
                    deps_merger = match deps_merger {
                        DepsMerger::None => DepsMerger::One(deps.dupe()),
                        DepsMerger::One(first_deps) => {
                            let mut builder = first_deps.into_builder();
                            builder.merge(deps.dupe().into_builder())?;
                            DepsMerger::Multiple(builder)
                        }
                        DepsMerger::Multiple(mut builder) => {
                            builder.merge(deps.dupe().into_builder())?;
                            DepsMerger::Multiple(builder)
                        }
                    }
                }
            }
            let entries = entries.into_iter().collect();

            let digest_config = ctx.global_data().get_digest_config();
            let d: DirectoryData<_, _, _> =
                DirectoryData::new(entries, digest_config.as_directory_serializer());
            let d = INTERNER.intern(d);

            let deps = match deps_merger {
                DepsMerger::None => None,
                DepsMerger::One(deps) => Some(deps),
                DepsMerger::Multiple(builder) => Some(
                    builder
                        .fingerprint(digest_config.as_directory_serializer())
                        .shared(&*INTERNER),
                ),
            };

            Ok(ArtifactValue::new(ActionDirectoryEntry::Dir(d), deps))
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            match (x, y) {
                (Ok(x), Ok(y)) => x == y,
                _ => false,
            }
        }
    }

    ctx.compute(&DirArtifactValueKey(cell_path)).await?
}

#[async_recursion]
async fn path_artifact_value(
    ctx: &mut DiceComputations<'_>,
    cell_path: Arc<CellPath>,
    label: Option<PackageLabel>,
) -> buck2_error::Result<ArtifactValue> {
    let raw = match DiceFileComputations::read_path_metadata(ctx, cell_path.as_ref().as_ref()).await
    {
        Ok(raw) => Ok(raw),
        Err(e) => {
            if let Some(label) = label {
                if let Ok(listing) = DicePackageListingResolver(ctx)
                    .resolve_package_listing(label.dupe())
                    .await
                {
                    return Err(e.with_package_context_information(
                        BuildFilePath::new(label, listing.buildfile().to_owned())
                            .path()
                            .path()
                            .to_string(),
                    ));
                }
            }

            // Suggestion is best effort, don't want it to override the actual error
            Err(e.without_package_context_information())
        }
    }?;

    match raw {
        RawPathMetadata::Symlink {
            at: _,
            to: RawSymlink::External(external_symlink),
        } => Ok(ArtifactValue::new(
            ActionDirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(external_symlink)),
            None,
        )),
        RawPathMetadata::File(metadata) => Ok(ArtifactValue::new(
            ActionDirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)),
            None,
        )),
        RawPathMetadata::Directory => dir_artifact_value(ctx, cell_path).await,
        RawPathMetadata::Symlink {
            at,
            to: RawSymlink::Relative(target, target_rel),
        } => {
            // TODO (T126181780): This should have a limit on recursion.
            let target_artifact_value = path_artifact_value(ctx, target.dupe(), label).await?;
            let root_cell = ctx.get_cell_resolver().await?.root_cell();
            let use_correct_source_symlink_reading = ctx
                .parse_legacy_config_property(
                    root_cell,
                    BuckconfigKeyRef {
                        section: "buck2",
                        property: "use_correct_source_symlink_reading",
                    },
                )
                .await?
                .unwrap_or(true);
            // In the case where this is a source artifact like `dir/link/foo`, where the symlink is
            // actually at `link`, `ArtifactValue` doesn't have a representation for the kind of
            // thing that'd require, so we read through the symlink instead. We could enhance
            // `ArtifactValue` to make that possible, but Jakob isn't sure that's a good idea
            let dont_read_through_symlink = use_correct_source_symlink_reading && at == cell_path;
            if dont_read_through_symlink {
                let artifact_fs = ctx.get_artifact_fs().await?;
                let target_path = artifact_fs.resolve_cell_path((*target).as_ref())?;
                let mut builder = ActionDirectoryBuilder::empty();
                insert_artifact(&mut builder, target_path, &target_artifact_value)?;
                let deps = builder
                    .fingerprint(
                        ctx.global_data()
                            .get_digest_config()
                            .as_directory_serializer(),
                    )
                    .shared(&*INTERNER);
                Ok(ArtifactValue::new(
                    ActionDirectoryEntry::Leaf(ActionDirectoryMember::Symlink(target_rel.dupe())),
                    Some(deps),
                ))
            } else {
                Ok(target_artifact_value)
            }
        }
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum ProjectedArtifactError {
    #[error("The path `{0}` does not exist in the artifact `{1}`")]
    #[buck2(tag = buck2_error::ErrorTag::ProjectMissingPath)]
    MissingInProjectedArtifact(ForwardRelativePathBuf, BaseArtifactKind),
}

#[derive(Clone, Dupe, Eq, PartialEq, Hash, Display, Debug, Allocative, RefCast)]
#[repr(transparent)]
pub struct EnsureProjectedArtifactKey(pub(crate) ArtifactKind);

#[async_trait]
impl Key for EnsureProjectedArtifactKey {
    type Value = buck2_error::Result<ArtifactValue>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let ArtifactKind { base, path } = &self.0;

        if path.is_empty() {
            return Err(internal_error!(
                "EnsureProjectedArtifactKey with non-empty projected path"
            ));
        }

        let base_value = ensure_base_artifact_staged(ctx, base.dupe())
            .await?
            .unpack_single()?;
        let base_content_based_path_hash = base_value.content_based_path_hash();

        let artifact_fs = ctx.get_artifact_fs().await?;
        let digest_config = ctx.global_data().get_digest_config();

        let base_path = match base {
            BaseArtifactKind::Build(built) => {
                artifact_fs.resolve_build(built.get_path(), Some(&base_content_based_path_hash))?
            }
            BaseArtifactKind::Source(source) => artifact_fs.resolve_source(source.get_path())?,
        };

        let projected_path = base_path.join(path);

        let mut builder = ActionDirectoryBuilder::empty();
        insert_artifact(&mut builder, base_path, &base_value)?;

        let value = extract_artifact_value(&builder, &projected_path, digest_config)
            .with_buck_error_context(|| {
                format!("The path `{path}` cannot be projected in the artifact `{base}`. Are you calling project() on a symlink?")
            })?
            .ok_or_else(|| {
                ProjectedArtifactError::MissingInProjectedArtifact(path.to_buf(), base.dupe())
            })?;

        // Projected artifacts are located in the same directory as the base artifact, so we
        // need to store the same content based path hash in order to find them in the correct place.
        let value = value.with_content_based_path_hash(base_content_based_path_hash);

        Ok(value)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[derive(Clone, Dupe, Eq, PartialEq, Hash, Display, Debug, Allocative, RefCast)]
#[repr(transparent)]
pub struct EnsureTransitiveSetProjectionKey(pub TransitiveSetProjectionKey);

#[async_trait]
impl Key for EnsureTransitiveSetProjectionKey {
    type Value = buck2_error::Result<ArtifactGroupValues>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let set = self.0.key.lookup(ctx).await?;

        let artifact_fs = ctx.get_artifact_fs().await?;

        let projection_sub_inputs = set.get_projection_sub_inputs(self.0.projection)?;

        let sub_inputs: Vec<_> = tokio::task::unconstrained(KeepGoing::try_compute_join_all(
            ctx,
            projection_sub_inputs.iter(),
            |ctx, a| async move { a.resolved_artifact(ctx).await }.boxed(),
        ))
        .await?;

        let (values, children) = {
            // Compute the new inputs. Note that ordering here (and below) is important to ensure
            // stability of the ArtifactGroupValues we produce across executions, which try_compute_join_all preserves.
            let ready_inputs: Vec<_> = tokio::task::unconstrained(KeepGoing::try_compute_join_all(
                ctx,
                sub_inputs.iter(),
                |ctx, v| async move { ensure_artifact_group_staged(ctx, v.clone()).await }.boxed(),
            ))
            .await?;

            // Partition our inputs in artifacts and projections.
            let mut values_count = 0;
            for input in sub_inputs.iter() {
                if let ResolvedArtifactGroup::Artifact(..) = input {
                    values_count += 1;
                }
            }

            let mut values = SmallVec::<[_; 1]>::with_capacity(values_count);
            let mut children = Vec::with_capacity(sub_inputs.len() - values_count);

            for (group, ready) in zip(sub_inputs.iter(), ready_inputs) {
                match group {
                    ResolvedArtifactGroup::Artifact(artifact) => {
                        values.push((artifact.dupe(), ready.unpack_single()?))
                    }
                    ResolvedArtifactGroup::TransitiveSetProjection(..) => {
                        children.push(ready.to_group_values(group)?)
                    }
                }
            }
            (values, children)
        };

        // At this point we're holding a lot of data and want to ensure that we don't hold that across any
        // .await, so move into a little sync closure and call that
        (move || {
            let digest_config = ctx.global_data().get_digest_config();

            let values = ArtifactGroupValues::new(values, children, &artifact_fs, digest_config)
                .buck_error_context("Failed to construct ArtifactGroupValues")?;

            Ok(values)
        })()
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x.shallow_equals(y),
            _ => false,
        }
    }
}
