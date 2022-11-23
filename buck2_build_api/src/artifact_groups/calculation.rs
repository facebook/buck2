/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use allocative::Allocative;
use anyhow::Context as _;
use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::file_ops::PathMetadata;
use buck2_common::file_ops::PathMetadataOrRedirection;
use buck2_common::result::SharedResult;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::directory::DirectoryData;
use buck2_execute::artifact_value::ArtifactValue;
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
use futures::future;
use futures::stream::FuturesOrdered;
use gazebo::prelude::*;
use smallvec::SmallVec;

use crate::actions::artifact::projected_artifact::ProjectedArtifact;
use crate::actions::artifact::Artifact;
use crate::actions::artifact::ArtifactKind;
use crate::actions::artifact::BaseArtifactKind;
use crate::actions::build_listener::HasBuildSignals;
use crate::actions::build_listener::TransitiveSetComputationSignal;
use crate::actions::calculation::ActionCalculation;
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
        // TODO consider if we need to cache this

        let res = match input {
            ArtifactGroup::Artifact(artifact) => {
                let value = ensure_artifact(self, artifact).await?;
                ArtifactGroupValues::from_artifact(artifact.dupe(), value)
            }
            ArtifactGroup::TransitiveSetProjection(key) => {
                self.compute(&EnsureTransitiveSetProjectionKey(key.dupe()))
                    .await??
            }
        };

        Ok(res)
    }
}

#[async_recursion]
async fn path_artifact_value(
    file_ops: &dyn FileOps,
    cell_path: &CellPath,
) -> anyhow::Result<ActionDirectoryEntry<ActionSharedDirectory>> {
    let raw = file_ops.read_path_metadata(cell_path).await?;
    match PathMetadataOrRedirection::from(raw) {
        PathMetadataOrRedirection::PathMetadata(meta) => match meta {
            PathMetadata::ExternalSymlink(symlink) => Ok(ActionDirectoryEntry::Leaf(
                ActionDirectoryMember::ExternalSymlink(symlink),
            )),
            PathMetadata::File(metadata) => Ok(ActionDirectoryEntry::Leaf(
                ActionDirectoryMember::File(metadata),
            )),
            PathMetadata::Directory => {
                let files = file_ops.read_dir(cell_path).await?;

                let entries = files.iter().map(|x| async {
                    let value =
                        path_artifact_value(file_ops, &cell_path.join(&x.file_name)).await?;
                    anyhow::Ok((x.file_name.clone(), value))
                });

                let entries = future::try_join_all(entries).await?;
                let entries = entries.into_iter().collect();

                let d: DirectoryData<_, _, _> = DirectoryData::new(entries);
                Ok(ActionDirectoryEntry::Dir(INTERNER.intern(d)))
            }
        },
        PathMetadataOrRedirection::Redirection(r) => {
            // TODO (T126181780): This should have a limit on recursion.
            path_artifact_value(file_ops, r.as_ref()).await
        }
    }
}

async fn ensure_base_artifact(
    dice: &DiceComputations,
    artifact: &BaseArtifactKind,
) -> anyhow::Result<ArtifactValue> {
    match artifact {
        BaseArtifactKind::Build(ref built) => {
            let action_result = dice.build_artifact(built).await?;
            if let Some(value) = action_result.get(built.get_path()) {
                Ok(value.dupe())
            } else {
                panic!(
                    "Building an artifact didn't produce it. Expected `{:?}` but only have `{:?}`",
                    artifact, action_result
                )
            }
        }
        BaseArtifactKind::Source(ref source) => Ok(path_artifact_value(
            &dice.file_ops(),
            &source.get_path().to_cell_path(),
        )
        .await?
        .into()),
    }
}

async fn ensure_artifact(
    dice: &DiceComputations,
    artifact: &Artifact,
) -> anyhow::Result<ArtifactValue> {
    Ok(match artifact.0.key() {
        ArtifactKind::Base(ref base) => ensure_base_artifact(dice, base).await?,
        ArtifactKind::Projected(projected) => {
            dice.compute(&EnsureProjectedArtifactKey(projected.dupe()))
                .await??
        }
    })
}

#[derive(Clone, Dupe, Eq, PartialEq, Hash, Display, Debug, Allocative)]
struct EnsureProjectedArtifactKey(ProjectedArtifact);

#[async_trait]
impl Key for EnsureProjectedArtifactKey {
    type Value = SharedResult<ArtifactValue>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let base_value = ensure_base_artifact(ctx, self.0.base()).await?;

        let artifact_fs = crate::calculation::Calculation::get_artifact_fs(ctx).await?;

        let base_path = match self.0.base() {
            BaseArtifactKind::Build(built) => artifact_fs.resolve_build(built.get_path()),
            BaseArtifactKind::Source(source) => artifact_fs.resolve_source(source)?,
        };

        let mut builder = ActionDirectoryBuilder::empty();
        insert_artifact(&mut builder, base_path.as_ref(), &base_value)?;

        let value = extract_artifact_value(&builder, base_path.join(self.0.path()).as_ref())?
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

#[derive(Clone, Dupe, Eq, PartialEq, Hash, Display, Debug, Allocative)]
struct EnsureTransitiveSetProjectionKey(TransitiveSetProjectionKey);

#[async_trait]
impl Key for EnsureTransitiveSetProjectionKey {
    type Value = SharedResult<ArtifactGroupValues>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let set = ctx
            .compute_deferred_data(&self.0.key)
            .await
            .context("Failed to compute deferred")?;

        let sub_inputs = set
            .as_transitive_set()?
            .get_projection_sub_inputs(self.0.projection)?;

        // Partition our inputs in artifacts and projections.
        let mut artifacts = Vec::new();
        let mut projections = Vec::new();

        for input in sub_inputs {
            match input {
                ArtifactGroup::Artifact(a) => artifacts.push(a),
                ArtifactGroup::TransitiveSetProjection(key) => {
                    projections.push(EnsureTransitiveSetProjectionKey(key))
                }
            };
        }

        // Compute the new inputs. Note that ordering here (and below) is important to ensure
        // stability of the ArtifactGroupValues we produce across executions, so we use
        // FuturesOrdered.

        let values = keep_going::try_join_all(
            artifacts
                .into_iter()
                .map(|a| async move {
                    let value = ensure_artifact(ctx, &a).await?;
                    anyhow::Ok((a, value))
                })
                .collect::<FuturesOrdered<_>>(),
        );

        let children = keep_going::try_join_all(
            projections
                .iter()
                .map(|key| async move { Ok(ctx.compute(key).await??) })
                .collect::<FuturesOrdered<_>>(),
        );

        let (values, children): (SmallVec<[_; 1]>, Vec<_>) =
            keep_going::try_join(values, children).await?;

        if let Some(build_signals) = ctx.per_transaction_data().get_build_signals() {
            let artifacts = values
                .iter()
                .filter_map(|(artifact, _value)| artifact.action_key().duped())
                .collect::<HashSet<_>>();

            let set_deps = projections.into_iter().map(|p| p.0).collect::<HashSet<_>>();

            build_signals.signal(TransitiveSetComputationSignal {
                key: self.0.dupe(),
                artifacts,
                set_deps,
            });
        }

        let artifact_fs = crate::calculation::Calculation::get_artifact_fs(ctx).await?;
        let values = ArtifactGroupValues::new(values, children, &artifact_fs)
            .context("Failed to construct ArtifactGroupValues")?;

        Ok(values)
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

    use buck2_common::dice::cells::HasCellResolver;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::dice::file_ops::keys::FileOpsValue;
    use buck2_common::dice::file_ops::testing::FileOpsKey;
    use buck2_common::file_ops::testing::TestFileOps;
    use buck2_common::file_ops::FileDigest;
    use buck2_common::file_ops::FileMetadata;
    use buck2_common::file_ops::TrackedFileDigest;
    use buck2_common::result::ToSharedResultExt;
    use buck2_core::buck_path::BuckPath;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::paths::CellRelativePathBuf;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_execute::artifact::source_artifact::SourceArtifact;
    use buck2_execute::artifact_value::ArtifactValue;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use indoc::indoc;
    use maplit::btreemap;
    use starlark::values::OwnedFrozenValue;

    use super::*;
    use crate::actions::artifact::Artifact;
    use crate::artifact_groups::deferred::DeferredTransitiveSetData;
    use crate::context::SetBuildContextData;
    use crate::deferred::calculation::testing::DeferredResolve;
    use crate::deferred::types::AnyValue;
    use crate::interpreter::rule_defs::transitive_set::testing;
    use crate::interpreter::rule_defs::transitive_set::TransitiveSet;
    use crate::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;

    fn mock_deferred_tset(dice_builder: DiceBuilder, value: OwnedFrozenValue) -> DiceBuilder {
        let tset = TransitiveSet::from_value(value.value()).unwrap();
        let resolve = DeferredResolve(tset.key().deferred_key().dupe());

        let data: Arc<dyn AnyValue + 'static> = Arc::new(DeferredTransitiveSetData(value));
        dice_builder.mock_and_return(resolve, anyhow::Ok(data).shared_error())
    }

    #[tokio::test]
    async fn test_ensure_artifact_group() -> anyhow::Result<()> {
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

        let cell_resolver = CellResolver::of_names_and_paths(&[(
            CellName::unchecked_new("".into()),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell-path".into())),
        )]);

        let foo = CellPath::new(
            CellName::unchecked_new("".to_owned()),
            CellRelativePathBuf::unchecked_new("foo/foo".to_owned()),
        );

        let foo_artifact = Artifact::from(SourceArtifact::new(BuckPath::new(
            Package::testing_new("", "foo"),
            PackageRelativePathBuf::unchecked_new("foo".to_owned()),
        )));

        let foo_meta = FileMetadata {
            digest: TrackedFileDigest::new(FileDigest::from_bytes_sha1(b"foo")),
            is_executable: true,
        };

        let bar_artifact = Artifact::from(SourceArtifact::new(BuckPath::new(
            Package::testing_new("", "bar"),
            PackageRelativePathBuf::unchecked_new("bar".to_owned()),
        )));

        let bar = CellPath::new(
            CellName::unchecked_new("".to_owned()),
            CellRelativePathBuf::unchecked_new("bar/bar".to_owned()),
        );

        let bar_meta = FileMetadata {
            digest: TrackedFileDigest::new(FileDigest::from_bytes_sha1(b"bar")),
            is_executable: true,
        };

        let files = TestFileOps::new_with_files_metadata(btreemap![
            foo => foo_meta.dupe(),
            bar => bar_meta.dupe(),
        ]);

        let fs = ProjectRootTemp::new()?;

        let mut dice_builder = DiceBuilder::new()
            .mock_and_return(FileOpsKey(), Ok(FileOpsValue(Arc::new(files))))
            .set_data(|data| data.set_testing_io_provider(&fs));

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

        let dice = dice_builder.build(UserComputationData::new())?;
        dice.set_cell_resolver(cell_resolver)?;
        dice.set_buck_out_path(None)?;
        let dice = dice.commit();

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
