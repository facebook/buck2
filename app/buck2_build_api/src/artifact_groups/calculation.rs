/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::iter::zip;

use allocative::Allocative;
use anyhow::Context as _;
use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::file_ops::PathMetadata;
use buck2_common::file_ops::PathMetadataOrRedirection;
use buck2_common::result::SharedResult;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::directory::DirectoryData;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::directory::extract_artifact_value;
use buck2_execute::directory::insert_artifact;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::directory::INTERNER;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future;
use futures::stream::FuturesOrdered;
use futures::Future;
use futures::FutureExt;
use ref_cast::RefCast;
use smallvec::SmallVec;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::projected_artifact::ProjectedArtifact;
use crate::actions::artifact::source_artifact::SourceArtifact;
use crate::actions::artifact::Artifact;
use crate::actions::artifact::ArtifactKind;
use crate::actions::artifact::BaseArtifactKind;
use crate::actions::build_listener::HasBuildSignals;
use crate::actions::build_listener::TransitiveSetComputationSignal;
use crate::actions::calculation::ActionCalculation;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::deferred::calculation::DeferredCalculation;
use crate::keep_going;

#[async_trait]
pub(crate) trait ArtifactGroupCalculation {
    /// Makes an 'Artifact' available to be accessed
    async fn ensure_artifact_group(
        &self,
        input: &ArtifactGroup,
    ) -> anyhow::Result<ArtifactGroupValues>;
}

#[async_trait]
impl ArtifactGroupCalculation for DiceComputations {
    /// makes the 'Artifact' available to be accessed
    async fn ensure_artifact_group(
        &self,
        input: &ArtifactGroup,
    ) -> anyhow::Result<ArtifactGroupValues> {
        ensure_artifact_group_staged(self, input)
            .await?
            .to_group_values(input)
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
    ctx: &'a DiceComputations,
    input: &'a ArtifactGroup,
) -> impl Future<Output = anyhow::Result<EnsureArtifactGroupReady>> + 'a {
    match input {
        ArtifactGroup::Artifact(artifact) => ensure_artifact_staged(ctx, artifact).left_future(),
        ArtifactGroup::TransitiveSetProjection(key) => ctx
            .compute(EnsureTransitiveSetProjectionKey::ref_cast(key))
            .map(|v| Ok(EnsureArtifactGroupReady::TransitiveSet(v??)))
            .right_future(),
    }
}

/// See [ensure_artifact_group_staged].
pub(super) fn ensure_base_artifact_staged<'a>(
    dice: &'a DiceComputations,
    artifact: &'a BaseArtifactKind,
) -> impl Future<Output = anyhow::Result<EnsureArtifactGroupReady>> + 'a {
    match artifact {
        BaseArtifactKind::Build(built) => ensure_build_artifact_staged(dice, built).left_future(),
        BaseArtifactKind::Source(source) => {
            ensure_source_artifact_staged(dice, source).right_future()
        }
    }
}

/// See [ensure_artifact_group_staged].
pub(super) fn ensure_artifact_staged<'a>(
    dice: &'a DiceComputations,
    artifact: &'a Artifact,
) -> impl Future<Output = anyhow::Result<EnsureArtifactGroupReady>> + 'a {
    match artifact.data() {
        ArtifactKind::Base(base) => ensure_base_artifact_staged(dice, base).left_future(),
        ArtifactKind::Projected(projected) => dice
            .compute(EnsureProjectedArtifactKey::ref_cast(projected))
            .map(|v| Ok(EnsureArtifactGroupReady::Single(v??)))
            .right_future(),
    }
}

fn ensure_build_artifact_staged<'a>(
    dice: &'a DiceComputations,
    built: &'a BuildArtifact,
) -> impl Future<Output = anyhow::Result<EnsureArtifactGroupReady>> + 'a {
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
    dice: &'a DiceComputations,
    source: &'a SourceArtifact,
) -> impl Future<Output = anyhow::Result<EnsureArtifactGroupReady>> + 'a {
    let digest_config = dice.global_data().get_digest_config();

    async move {
        Ok(EnsureArtifactGroupReady::Single(
            path_artifact_value(
                &dice.file_ops(),
                source.get_path().to_cell_path().as_ref(),
                digest_config,
            )
            .await?
            .into(),
        ))
    }
    .boxed()
}

// These errors should be unreachable, they indicate misuse of the staged ensure artifact (or other buck
// invariant violations), but it's still better to propagate them as Error than to panic!().
#[derive(Debug, Error)]
pub enum EnsureArtifactStagedError {
    #[error("Tried to unpack single artifact, but got transitive set")]
    UnpackSingleTransitiveSet,
    #[error("Expected a transitive set, got a single artifact")]
    ExpectedTransitiveSet,
    // This one could probably be a panic! if DICE didn't eagerly re-evaluate all deps.
    #[error("Building an artifact didn't produce it. Expected `{0}` but only have `{1:?}`")]
    BuildArtifactMissing(BuildArtifact, ActionOutputs),
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
    pub(crate) fn to_group_values(
        self,
        artifact: &ArtifactGroup,
    ) -> anyhow::Result<ArtifactGroupValues> {
        match self {
            EnsureArtifactGroupReady::TransitiveSet(values) => Ok(values),
            EnsureArtifactGroupReady::Single(value) => match artifact {
                ArtifactGroup::Artifact(artifact) => {
                    Ok(ArtifactGroupValues::from_artifact(artifact.clone(), value))
                }
                ArtifactGroup::TransitiveSetProjection(_) => {
                    Err(EnsureArtifactStagedError::ExpectedTransitiveSet.into())
                }
            },
        }
    }

    fn unpack_single(self) -> anyhow::Result<ArtifactValue> {
        match self {
            EnsureArtifactGroupReady::Single(value) => Ok(value),
            EnsureArtifactGroupReady::TransitiveSet(..) => {
                Err(EnsureArtifactStagedError::UnpackSingleTransitiveSet.into())
            }
        }
    }
}

static_assertions::assert_eq_size!(EnsureArtifactGroupReady, [usize; 3]);

// This assertion assures we don't unknowingly regress the size of this critical future.
// TODO(cjhopman): We should be able to wrap this in a convenient assertion macro.
#[allow(unused, clippy::diverging_sub_expression)]
fn _assert_ensure_artifact_group_future_size() {
    let v = ensure_artifact_group_staged(panic!(), panic!());
    let e = [0u8; 704 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);
}

async fn dir_artifact_value(
    file_ops: &dyn FileOps,
    cell_path: CellPathRef<'_>,
    digest_config: DigestConfig,
) -> anyhow::Result<ActionDirectoryEntry<ActionSharedDirectory>> {
    let files = file_ops.read_dir(cell_path.dupe()).await?;

    let entries = files.iter().map(|x| async {
        let value = path_artifact_value(
            file_ops,
            cell_path.join(&x.file_name).as_ref(),
            digest_config,
        )
        .await?;
        anyhow::Ok((x.file_name.clone(), value))
    });

    let entries = future::try_join_all(entries).await?;
    let entries = entries.into_iter().collect();

    let d: DirectoryData<_, _, _> =
        DirectoryData::new(entries, digest_config.as_directory_serializer());
    Ok(ActionDirectoryEntry::Dir(INTERNER.intern(d)))
}

#[async_recursion]
async fn path_artifact_value(
    file_ops: &dyn FileOps,
    cell_path: CellPathRef<'async_recursion>,
    digest_config: DigestConfig,
) -> anyhow::Result<ActionDirectoryEntry<ActionSharedDirectory>> {
    let raw = file_ops.read_path_metadata(cell_path.dupe()).await?;
    match PathMetadataOrRedirection::from(raw) {
        PathMetadataOrRedirection::PathMetadata(meta) => match meta {
            PathMetadata::ExternalSymlink(symlink) => Ok(ActionDirectoryEntry::Leaf(
                ActionDirectoryMember::ExternalSymlink(symlink),
            )),
            PathMetadata::File(metadata) => Ok(ActionDirectoryEntry::Leaf(
                ActionDirectoryMember::File(metadata),
            )),
            PathMetadata::Directory => dir_artifact_value(file_ops, cell_path, digest_config).await,
        },
        PathMetadataOrRedirection::Redirection(r) => {
            // TODO (T126181780): This should have a limit on recursion.
            path_artifact_value(file_ops, r.as_ref().as_ref(), digest_config).await
        }
    }
}

#[derive(Clone, Dupe, Eq, PartialEq, Hash, Display, Debug, Allocative, RefCast)]
#[repr(transparent)]
pub(crate) struct EnsureProjectedArtifactKey(ProjectedArtifact);

#[async_trait]
impl Key for EnsureProjectedArtifactKey {
    type Value = SharedResult<ArtifactValue>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let base_value = ensure_base_artifact_staged(ctx, self.0.base())
            .await?
            .unpack_single()?;

        let artifact_fs = crate::calculation::Calculation::get_artifact_fs(ctx).await?;
        let digest_config = ctx.global_data().get_digest_config();

        let base_path = match self.0.base() {
            BaseArtifactKind::Build(built) => artifact_fs.resolve_build(built.get_path()),
            BaseArtifactKind::Source(source) => artifact_fs.resolve_source(source.get_path())?,
        };

        let mut builder = ActionDirectoryBuilder::empty();
        insert_artifact(&mut builder, base_path.as_ref(), &base_value)?;

        let value =
            extract_artifact_value(&builder, &base_path.join(self.0.path()), digest_config)?
                .with_context(|| {
                    format!(
                        "The path `{}` does not exist in the artifact `{}`",
                        self.0.path(),
                        self.0.base()
                    )
                })?;

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
pub(crate) struct EnsureTransitiveSetProjectionKey(TransitiveSetProjectionKey);

#[async_trait]
impl Key for EnsureTransitiveSetProjectionKey {
    type Value = SharedResult<ArtifactGroupValues>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let set = ctx
            .compute_deferred_data(&self.0.key)
            .await
            .context("Failed to compute deferred")?;

        let artifact_fs = crate::calculation::Calculation::get_artifact_fs(ctx).await?;

        let sub_inputs = set
            .as_transitive_set()?
            .get_projection_sub_inputs(self.0.projection)?;

        let (values, children) = {
            // Compute the new inputs. Note that ordering here (and below) is important to ensure
            // stability of the ArtifactGroupValues we produce across executions, so we use
            // FuturesOrdered.

            let ensure_futs: FuturesOrdered<_> = sub_inputs
                .iter()
                .map(|v| ensure_artifact_group_staged(ctx, v))
                .collect();

            let ready_inputs: Vec<_> =
                tokio::task::unconstrained(keep_going::try_join_all(ensure_futs)).await?;

            // Partition our inputs in artifacts and projections.
            let mut values_count = 0;
            for input in sub_inputs.iter() {
                if let ArtifactGroup::Artifact(..) = input {
                    values_count += 1;
                }
            }

            let mut values = SmallVec::<[_; 1]>::with_capacity(values_count);
            let mut children = Vec::with_capacity(sub_inputs.len() - values_count);

            for (group, ready) in zip(sub_inputs.iter(), ready_inputs.into_iter()) {
                match group {
                    ArtifactGroup::Artifact(artifact) => {
                        values.push((artifact.dupe(), ready.unpack_single()?))
                    }
                    ArtifactGroup::TransitiveSetProjection(..) => {
                        children.push(ready.to_group_values(group)?)
                    }
                }
            }
            (values, children)
        };

        // At this point we're holding a lot of data and want to ensure that we don't hold that across any
        // .await, so move into a little sync closure and call that
        (move || {
            if let Some(build_signals) = ctx.per_transaction_data().get_build_signals() {
                let mut artifacts = HashSet::new();
                let mut set_deps = HashSet::new();

                for input in sub_inputs.iter() {
                    match input {
                        ArtifactGroup::Artifact(artifact) => {
                            if let Some(key) = artifact.action_key() {
                                artifacts.insert(key.clone());
                            }
                        }
                        ArtifactGroup::TransitiveSetProjection(tset) => {
                            set_deps.insert(tset.clone());
                        }
                    }
                }

                build_signals.signal(TransitiveSetComputationSignal {
                    key: self.0.dupe(),
                    artifacts,
                    set_deps,
                });
            }

            let digest_config = ctx.global_data().get_digest_config();

            let values = ArtifactGroupValues::new(values, children, &artifact_fs, digest_config)
                .context("Failed to construct ArtifactGroupValues")?;

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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_common::dice::cells::SetCellResolver;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::dice::file_ops::keys::FileOpsValue;
    use buck2_common::dice::file_ops::testing::FileOpsKey;
    use buck2_common::file_ops::testing::TestFileOps;
    use buck2_common::file_ops::FileMetadata;
    use buck2_common::file_ops::TrackedFileDigest;
    use buck2_common::result::ToSharedResultExt;
    use buck2_core::buck_path::path::BuckPath;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePathBuf;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellResolver;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::PackageLabel;
    use buck2_execute::artifact_value::ArtifactValue;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::digest_config::SetDigestConfig;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use indoc::indoc;
    use maplit::btreemap;
    use starlark::values::OwnedFrozenValue;

    use super::*;
    use crate::actions::artifact::source_artifact::SourceArtifact;
    use crate::actions::artifact::Artifact;
    use crate::artifact_groups::deferred::DeferredTransitiveSetData;
    use crate::context::SetBuildContextData;
    use crate::deferred::calculation::testing::DeferredResolve;
    use crate::deferred::types::AnyValue;
    use crate::deferred::types::DeferredValueAnyReady;
    use crate::interpreter::rule_defs::transitive_set::testing;
    use crate::interpreter::rule_defs::transitive_set::TransitiveSet;
    use crate::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;

    fn mock_deferred_tset(dice_builder: DiceBuilder, value: OwnedFrozenValue) -> DiceBuilder {
        let tset = TransitiveSet::from_value(value.value()).unwrap();
        let resolve = DeferredResolve(tset.key().deferred_key().dupe());

        let data: Arc<dyn AnyValue + 'static> = Arc::new(DeferredTransitiveSetData(value));
        let any = DeferredValueAnyReady::AnyValue(data);
        dice_builder.mock_and_return(resolve, anyhow::Ok(any).shared_error())
    }

    #[tokio::test]
    async fn test_ensure_artifact_group() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let set = testing::new_transitive_set(indoc!(
            r#"
            def project(value):
                return value

            TestSet = transitive_set(args_projections = {
                "project": project
            })

            foo = source_artifact("foo", "foo")
            bar = source_artifact("bar", "bar")

            s1 = make_tset(TestSet, value = foo)
            make_tset(TestSet, value = bar, children = [s1])
            "#
        ))?;

        let heap = set.owner();

        let cell_resolver = CellResolver::of_names_and_paths(
            CellName::testing_new("root"),
            &[(
                CellName::testing_new("root"),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell-path".into())),
            )],
        );

        let foo = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePathBuf::unchecked_new("foo/foo".to_owned()),
        );

        let foo_artifact = Artifact::from(SourceArtifact::new(BuckPath::testing_new(
            PackageLabel::testing_parse("root//foo"),
            PackageRelativePathBuf::unchecked_new("foo".to_owned()),
        )));

        let foo_meta = FileMetadata {
            digest: TrackedFileDigest::from_content(b"foo", digest_config.cas_digest_config()),
            is_executable: true,
        };

        let bar_artifact = Artifact::from(SourceArtifact::new(BuckPath::testing_new(
            PackageLabel::testing_parse("root//bar"),
            PackageRelativePathBuf::unchecked_new("bar".to_owned()),
        )));

        let bar = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePathBuf::unchecked_new("bar/bar".to_owned()),
        );

        let bar_meta = FileMetadata {
            digest: TrackedFileDigest::from_content(b"bar", digest_config.cas_digest_config()),
            is_executable: true,
        };

        let files = TestFileOps::new_with_files_metadata(btreemap![
            foo => foo_meta.dupe(),
            bar => bar_meta.dupe(),
        ]);

        let fs = ProjectRootTemp::new()?;

        let mut dice_builder = DiceBuilder::new()
            .mock_and_return(FileOpsKey(), Ok(FileOpsValue(Arc::new(files))))
            .set_data(|data| {
                data.set_testing_io_provider(&fs);
                data.set_digest_config(DigestConfig::testing_default());
            });

        // Register all the sets as deferreds.
        dice_builder = mock_deferred_tset(dice_builder, set.to_owned_frozen_value());

        // This is kinda clowny, but we can't upcast the TransitiveSetGen back to a Value so we
        // have to access Values from their parents.
        for set in set.as_ref().iter(TransitiveSetOrdering::Preorder) {
            for child in set.children.iter() {
                // Safety: We know the entire set came from the same heap.
                let child = unsafe { OwnedFrozenValue::new(heap.dupe(), *child) };
                dice_builder = mock_deferred_tset(dice_builder, child);
            }
        }

        let mut dice = dice_builder.build(UserComputationData::new())?;
        dice.set_cell_resolver(cell_resolver)?;
        dice.set_buck_out_path(None)?;
        let dice = dice.commit().await;

        let result = dice
            .ensure_artifact_group(&ArtifactGroup::TransitiveSetProjection(
                TransitiveSetProjectionKey {
                    key: set.key.dupe(),
                    projection: 0,
                },
            ))
            .await?
            .iter()
            .cloned()
            .collect::<Vec<_>>();

        assert_eq!(
            &result,
            &[
                (
                    bar_artifact,
                    ArtifactValue::file(FileMetadata {
                        digest: bar_meta.digest,
                        is_executable: bar_meta.is_executable,
                    })
                ),
                (
                    foo_artifact,
                    ArtifactValue::file(FileMetadata {
                        digest: foo_meta.digest,
                        is_executable: foo_meta.is_executable,
                    })
                ),
            ]
        );

        Ok(())
    }
}
