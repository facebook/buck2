/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Arc;

use buck2_analysis::analysis::calculation::AnalysisKey;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_build_api::actions::registry::RecordedActions;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::analysis::registry::RecordedAnalysisValues;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::TransitiveSetProjectionKey;
use buck2_build_api::artifact_groups::TransitiveSetProjectionWrapper;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::artifact_groups::deferred::TransitiveSetKey;
use buck2_build_api::context::SetBuildContextData;
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSetOrdering;
use buck2_build_api::keep_going::HasKeepGoing;
use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::testing::SetTestingIoProvider;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_common::file_ops::testing::TestFileOps;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::dice::inject_legacy_config_for_test;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::package::source_path::SourcePath;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use dice::UserComputationData;
use dice::testing::DiceBuilder;
use dupe::Dupe;
use indoc::indoc;
use maplit::btreemap;
use starlark::values::OwnedFrozenValue;
use starlark::values::OwnedFrozenValueTyped;

use crate::interpreter::transitive_set::testing::TSET_TEST_LOCK;
use crate::interpreter::transitive_set::testing::new_transitive_set;

fn mock_analysis_for_tsets(
    mut dice_builder: DiceBuilder,
    tsets: Vec<OwnedFrozenValueTyped<FrozenTransitiveSet>>,
) -> DiceBuilder {
    let mut by_target: HashMap<
        ConfiguredTargetLabel,
        Vec<(TransitiveSetKey, OwnedFrozenValueTyped<FrozenTransitiveSet>)>,
    > = HashMap::new();

    for value in tsets {
        let key = value.key().dupe();
        match key.holder_key() {
            DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(target)) => {
                by_target
                    .entry(target.dupe())
                    .or_default()
                    .push((key, value.dupe()));
            }
            _ => unreachable!("we only make fake tsets with configured targets `{}`", key),
        }
    }

    for (target, tsets) in by_target.into_iter() {
        dice_builder = dice_builder.mock_and_return(
            AnalysisKey(target.dupe()),
            buck2_error::Ok(MaybeCompatible::Compatible(AnalysisResult::new(
                RecordedAnalysisValues::testing_new(
                    DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(target)),
                    tsets,
                    RecordedActions::new(0),
                ),
                None,
                HashMap::new(),
                0,
                0,
                None,
            ))),
        );
    }

    dice_builder
}

#[tokio::test]
async fn test_ensure_artifact_group() -> buck2_error::Result<()> {
    // Serialize with other tests that use make_tset() and its shared global counter
    let _guard = TSET_TEST_LOCK.lock().unwrap();

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

            def make():
                s1 = make_tset(TestSet, value = foo)
                return make_tset(TestSet, value = bar, children = [s1])
            "#
    ))?;

    let heap = set.owner();

    let cell_resolver = CellResolver::testing_with_names_and_paths(&[
        (
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new("cell-path"),
        ),
        (
            CellName::testing_new("parent"),
            CellRootPathBuf::testing_new(""),
        ),
    ]);

    let foo = CellPath::new(
        CellName::testing_new("root"),
        CellRelativePathBuf::unchecked_new("foo/foo".to_owned()),
    );

    let foo_artifact = Artifact::from(SourceArtifact::new(SourcePath::testing_new(
        "root//foo",
        "foo",
    )));

    let foo_meta = FileMetadata {
        digest: TrackedFileDigest::from_content(b"foo", digest_config.cas_digest_config()),
        is_executable: true,
    };

    let bar_artifact = Artifact::from(SourceArtifact::new(SourcePath::testing_new(
        "root//bar",
        "bar",
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

    let cell_root = CellName::testing_new("root");
    let cell_parent = CellName::testing_new("parent");
    let dice_builder = files.mock_in_cell(cell_root, DiceBuilder::new());
    let dice_builder = files.mock_in_cell(cell_parent, dice_builder);
    let mut dice_builder = dice_builder.set_data(|data| {
        data.set_testing_io_provider(&fs);
        data.set_digest_config(DigestConfig::testing_default());
    });

    let mut all_tsets = vec![set.dupe()];
    // This is kinda clowny, but we can't upcast the TransitiveSetGen back to a Value so we
    // have to access Values from their parents.
    for set in set.as_ref().iter(TransitiveSetOrdering::Preorder) {
        for child in set.children.iter() {
            // Safety: We know the entire set came from the same heap.
            let child = unsafe { OwnedFrozenValue::new(heap.dupe(), *child) };
            all_tsets.push(child.downcast().unwrap());
        }
    }

    // Register all the sets as deferreds.
    dice_builder = mock_analysis_for_tsets(dice_builder, all_tsets);

    let mut extra = UserComputationData::new();
    extra.set_keep_going(true);

    let mut dice = dice_builder.build(extra).unwrap();
    dice.set_cell_resolver(cell_resolver)?;
    dice.set_buck_out_path(None)?;
    inject_legacy_config_for_test(&mut dice, cell_parent, LegacyBuckConfig::empty())?;
    let mut dice = dice.commit().await;

    let result = dice
        .ensure_artifact_group(&ArtifactGroup::TransitiveSetProjection(Arc::new(
            TransitiveSetProjectionWrapper::new(
                TransitiveSetProjectionKey {
                    key: set.key.dupe(),
                    projection: 0,
                },
                false,
                false,
            ),
        )))
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
