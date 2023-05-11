/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use assert_matches::assert_matches;
use buck2_build_api::actions::artifact::artifact_type::testing::ArtifactTestingExt;
use buck2_build_api::actions::artifact::artifact_type::testing::BuildArtifactTestingExt;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::registry::ActionsRegistry;
use buck2_build_api::actions::ActionErrors;
use buck2_build_api::analysis::registry::AnalysisValueFetcher;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
use buck2_build_api::deferred::types::testing::DeferredIdExt;
use buck2_build_api::deferred::types::BaseKey;
use buck2_build_api::deferred::types::DeferredId;
use buck2_build_api::deferred::types::DeferredRegistry;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_core::category::Category;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_execute::execute::request::OutputType;
use buck2_node::configuration::execution::ExecutionPlatform;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use dupe::Dupe;
use indexmap::indexset;

use crate::actions::testings::SimpleUnregisteredAction;

#[test]
fn declaring_artifacts() -> anyhow::Result<()> {
    let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
        "cell//pkg:foo",
        ConfigurationData::testing_new(),
    ));
    let mut actions = ActionsRegistry::new(base.dupe(), ExecutionPlatformResolution::unspecified());
    let out1 = ForwardRelativePathBuf::unchecked_new("bar.out".into());
    let buckout1 = BuckOutPath::new(base.dupe().into_dyn(), out1.clone());
    let declared1 = actions.declare_artifact(None, out1.clone(), OutputType::File, None)?;
    declared1
        .get_path()
        .with_full_path(|p| assert_eq!(p, buckout1.path()));

    let out2 = ForwardRelativePathBuf::unchecked_new("bar2.out".into());
    let buckout2 = BuckOutPath::new(base.into_dyn(), out2.clone());
    let declared2 = actions.declare_artifact(None, out2, OutputType::File, None)?;
    declared2
        .get_path()
        .with_full_path(|p| assert_eq!(p, buckout2.path()));

    if actions
        .declare_artifact(None, out1, OutputType::File, None)
        .is_ok()
    {
        panic!("should error due to duplicate artifact")
    }

    assert_eq!(actions.testing_artifacts().contains(&declared1), true);
    assert_eq!(actions.testing_artifacts().contains(&declared2), true);

    Ok(())
}

#[test]
fn claiming_conflicting_path() -> anyhow::Result<()> {
    let target = ConfiguredTargetLabel::testing_parse(
        "cell//pkg:my_target",
        ConfigurationData::testing_new(),
    );
    let mut actions = ActionsRegistry::new(
        BaseDeferredKey::TargetLabel(target.dupe()),
        ExecutionPlatformResolution::unspecified(),
    );

    let out1 = ForwardRelativePathBuf::unchecked_new("foo/a/1".into());
    actions.claim_output_path(&out1, None)?;

    let out2 = ForwardRelativePathBuf::unchecked_new("foo/a/2".into());
    actions.claim_output_path(&out2, None)?;

    {
        let expected_conflicts = vec!["foo/a/1 declared at <unknown>".to_owned()];
        let prefix_claimed = ForwardRelativePathBuf::unchecked_new("foo/a/1/some/path".into());
        assert_matches!(
            actions.claim_output_path(&prefix_claimed, None),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<ActionErrors>(),
                    Some(ActionErrors::ConflictingOutputPaths(_inserted, existing)) => {
                        assert_eq!(existing, &expected_conflicts);
                    }
                );
            }
        );
    }

    assert_matches!(
        actions.claim_output_path(&out1, None),
        Err(e) => {
            assert_matches!(
                e.downcast_ref::<ActionErrors>(),
                Some(ActionErrors::ConflictingOutputPath(_))
            );
        }
    );

    {
        let overwrite_dir = ForwardRelativePathBuf::unchecked_new("foo".into());
        let expected_conflicts = vec![
            "foo/a/1 declared at <unknown>".to_owned(),
            "foo/a/2 declared at <unknown>".to_owned(),
        ];
        assert_matches!(
            actions.claim_output_path(&overwrite_dir, None),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<ActionErrors>(),
                    Some(ActionErrors::ConflictingOutputPaths(_inserted, existing)) => {
                        assert_eq!(existing, &expected_conflicts);
                    }
                );
            }
        );
    }

    Ok(())
}

#[test]
fn register_actions() -> anyhow::Result<()> {
    let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
        "cell//pkg:foo",
        ConfigurationData::testing_new(),
    ));
    let mut deferreds = DeferredRegistry::new(BaseKey::Base(base.dupe()));
    let mut actions = ActionsRegistry::new(base.dupe(), ExecutionPlatformResolution::unspecified());
    let out = ForwardRelativePathBuf::unchecked_new("bar.out".into());
    let declared = actions.declare_artifact(None, out, OutputType::File, None)?;

    let inputs = indexset![ArtifactGroup::Artifact(
        BuildArtifact::testing_new(
            base.unpack_target_label().unwrap().dupe(),
            ForwardRelativePathBuf::unchecked_new("input".into()),
            DeferredId::testing_new(1),
        )
        .into()
    )];
    let outputs = indexset![declared.as_output()];

    let unregistered_action =
        SimpleUnregisteredAction::new(vec![], Category::try_from("fake_action").unwrap(), None);
    assert_eq!(
        actions
            .register(&mut deferreds, inputs, outputs, unregistered_action)
            .is_ok(),
        true
    );

    assert_eq!(actions.testing_pending().count(), 1);
    assert_eq!(declared.testing_is_bound(), true);
    assert_eq!(
        actions
            .testing_pending()
            .any(|reserved| reserved.data()
                == declared.testing_action_key().unwrap().deferred_data()),
        true
    );

    Ok(())
}

#[test]
fn finalizing_actions() -> anyhow::Result<()> {
    let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
        "cell//pkg:foo",
        ConfigurationData::testing_new(),
    ));
    let mut deferreds = DeferredRegistry::new(BaseKey::Base(base.dupe()));
    let mut actions = ActionsRegistry::new(
        base.dupe(),
        ExecutionPlatformResolution::new(
            Some(ExecutionPlatform::legacy_execution_platform(
                CommandExecutorConfig::testing_local(),
                ConfigurationNoExec::testing_new(),
            )),
            Vec::new(),
        ),
    );
    let out = ForwardRelativePathBuf::unchecked_new("bar.out".into());
    let declared = actions.declare_artifact(None, out, OutputType::File, None)?;

    let inputs = indexset![ArtifactGroup::Artifact(
        BuildArtifact::testing_new(
            base.unpack_target_label().unwrap().dupe(),
            ForwardRelativePathBuf::unchecked_new("input".into()),
            DeferredId::testing_new(1),
        )
        .into()
    )];
    let outputs = indexset![declared.as_output()];

    let unregistered_action =
        SimpleUnregisteredAction::new(vec![], Category::try_from("fake_action").unwrap(), None);
    actions.register(&mut deferreds, inputs, outputs, unregistered_action)?;

    let result = actions.ensure_bound(&mut deferreds, &AnalysisValueFetcher::default());
    assert_eq!(result.is_ok(), true, "Expected Ok(_), got `{:?}`", result);

    let registered_deferreds = deferreds.take_result()?;

    assert_eq!(registered_deferreds.len(), 1);

    assert_eq!(
        registered_deferreds
            .get(
                declared
                    .testing_action_key()
                    .unwrap()
                    .deferred_key()
                    .id()
                    .as_usize()
            )
            .is_some(),
        true
    );

    Ok(())
}

#[test]
fn duplicate_category_singleton_actions() {
    let result =
        category_identifier_test(&[("singleton_category", None), ("singleton_category", None)])
            .unwrap_err()
            .downcast::<ActionErrors>()
            .unwrap();

    assert!(matches!(
        result,
        ActionErrors::ActionCategoryDuplicateSingleton(_)
    ));
}

#[test]
fn duplicate_category_identifier() {
    let result = category_identifier_test(&[
        ("cxx_compile", Some("foo.cpp")),
        ("cxx_compile", Some("foo.cpp")),
    ])
    .unwrap_err()
    .downcast::<ActionErrors>()
    .unwrap();

    assert!(matches!(
        result,
        ActionErrors::ActionCategoryIdentifierNotUnique(_, _)
    ),);
}

fn category_identifier_test(
    action_names: &[(&'static str, Option<&'static str>)],
) -> anyhow::Result<()> {
    let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
        "cell//pkg:foo",
        ConfigurationData::testing_new(),
    ));
    let mut deferreds = DeferredRegistry::new(BaseKey::Base(base.dupe()));
    let mut actions = ActionsRegistry::new(
        base.dupe(),
        ExecutionPlatformResolution::new(
            Some(ExecutionPlatform::legacy_execution_platform(
                CommandExecutorConfig::testing_local(),
                ConfigurationNoExec::testing_new(),
            )),
            Vec::new(),
        ),
    );
    for (category, identifier) in action_names {
        let unregistered_action = SimpleUnregisteredAction::new(
            vec![],
            Category::try_from(category.to_owned()).unwrap(),
            identifier.map(|i| i.to_owned()),
        );

        actions.register(
            &mut deferreds,
            indexset![],
            indexset![],
            unregistered_action,
        )?;
    }

    actions.ensure_bound(&mut deferreds, &AnalysisValueFetcher::default())
}
