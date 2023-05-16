/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use buck2_build_api::actions::artifact::artifact_type::Artifact;
use buck2_build_api::actions::artifact::source_artifact::SourceArtifact;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::artifact_groups::deferred::DeferredTransitiveSetData;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::TransitiveSetProjectionKey;
use buck2_build_api::context::SetBuildContextData;
use buck2_build_api::deferred::calculation::DeferredResolve;
use buck2_build_api::deferred::types::AnyValue;
use buck2_build_api::deferred::types::DeferredValueAnyReady;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;
use buck2_build_api::keep_going::HasKeepGoing;
use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::testing::SetTestingIoProvider;
use buck2_common::dice::file_ops::keys::FileOpsKey;
use buck2_common::dice::file_ops::keys::FileOpsValue;
use buck2_common::file_ops::testing::TestFileOps;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::result::ToSharedResultExt;
use buck2_core::buck_path::path::BuckPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use dice::testing::DiceBuilder;
use dice::UserComputationData;
use dupe::Dupe;
use indoc::indoc;
use maplit::btreemap;
use starlark::values::OwnedFrozenValue;

use crate::interpreter::transitive_set::testing::new_transitive_set;

fn mock_deferred_tset(dice_builder: DiceBuilder, value: OwnedFrozenValue) -> DiceBuilder {
    let tset = TransitiveSet::from_value(value.value()).unwrap();
    let resolve = DeferredResolve(tset.key().deferred_key().dupe());

    let data: Arc<dyn AnyValue + 'static> = Arc::new(DeferredTransitiveSetData::testing_new(value));
    let any = DeferredValueAnyReady::AnyValue(data);
    dice_builder.mock_and_return(resolve, anyhow::Ok(any).shared_error())
}

#[tokio::test]
async fn test_ensure_artifact_group() -> anyhow::Result<()> {
    let digest_config = DigestConfig::testing_default();

    let set = new_transitive_set(indoc!(
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

    let cell_resolver = CellResolver::testing_with_names_and_paths_with_alias(&[
        (
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new("cell-path"),
            HashMap::new(),
        ),
        (
            CellName::testing_new("parent"),
            CellRootPathBuf::testing_new(""),
            HashMap::new(),
        ),
    ]);

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

    let mut extra = UserComputationData::new();
    extra.set_keep_going(true);

    let mut dice = dice_builder.build(extra)?;
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
