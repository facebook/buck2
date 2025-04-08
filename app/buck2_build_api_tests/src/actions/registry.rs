/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::artifact::artifact_type::testing::ArtifactTestingExt;
use buck2_artifact::artifact::artifact_type::testing::BuildArtifactTestingExt;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::registry::ActionsRegistry;
use buck2_build_api::actions::ActionErrors;
use buck2_build_api::analysis::registry::AnalysisValueFetcher;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_core::category::Category;
use buck2_core::category::CategoryRef;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::execution_types::execution::ExecutionPlatform;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_execute::execute::request::OutputType;
use dupe::Dupe;
use indexmap::indexset;
use itertools::Itertools;

use crate::actions::testings::SimpleUnregisteredAction;

#[test]
fn declaring_artifacts() -> anyhow::Result<()> {
    let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
        "cell//pkg:foo",
        ConfigurationData::testing_new(),
    ));
    let mut actions = ActionsRegistry::new(
        DeferredHolderKey::Base(base.dupe()),
        ExecutionPlatformResolution::unspecified(),
    );
    let out1 = ForwardRelativePathBuf::unchecked_new("bar.out".into());
    let buckout1 = BuildArtifactPath::new(base.dupe(), out1.clone());
    let declared1 = actions.declare_artifact(None, out1.clone(), OutputType::File, None)?;
    declared1
        .get_path()
        .with_full_path(|p| assert_eq!(p, buckout1.path()));

    let out2 = ForwardRelativePathBuf::unchecked_new("bar2.out".into());
    let buckout2 = BuildArtifactPath::new(base, out2.clone());
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
    let mut actions = ActionsRegistry::new(
        DeferredHolderKey::testing_new("cell//pkg:my_target"),
        ExecutionPlatformResolution::unspecified(),
    );

    let out1 = ForwardRelativePathBuf::unchecked_new("foo/a/1".into());
    actions.claim_output_path(&out1, None)?;

    let out2 = ForwardRelativePathBuf::unchecked_new("foo/a/2".into());
    actions.claim_output_path(&out2, None)?;

    {
        let expected_conflicts = vec!["foo/a/1 declared at <unknown>".to_owned()];
        let prefix_claimed = ForwardRelativePathBuf::unchecked_new("foo/a/1/some/path".into());

        let actual = actions
            .claim_output_path(&prefix_claimed, None)
            .unwrap_err();
        let expected: buck2_error::Error =
            ActionErrors::ConflictingOutputPaths(prefix_claimed, expected_conflicts).into();
        assert_eq!(actual.to_string(), expected.to_string());
    }

    let err = actions.claim_output_path(&out1, None).unwrap_err();
    assert!(
        err.category_key()
            .ends_with("ActionErrors::ConflictingOutputPath")
    );

    {
        let overwrite_dir = ForwardRelativePathBuf::unchecked_new("foo".into());
        let expected_conflicts = vec![
            "foo/a/1 declared at <unknown>".to_owned(),
            "foo/a/2 declared at <unknown>".to_owned(),
        ];

        let actual = actions.claim_output_path(&overwrite_dir, None).unwrap_err();
        let expected: buck2_error::Error =
            ActionErrors::ConflictingOutputPaths(overwrite_dir, expected_conflicts).into();
        assert_eq!(actual.to_string(), expected.to_string());
    }

    Ok(())
}

#[test]
fn register_actions() -> anyhow::Result<()> {
    let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
        "cell//pkg:foo",
        ConfigurationData::testing_new(),
    ));
    let mut actions = ActionsRegistry::new(
        DeferredHolderKey::Base(base.dupe()),
        ExecutionPlatformResolution::unspecified(),
    );
    let out = ForwardRelativePathBuf::unchecked_new("bar.out".into());
    let declared = actions.declare_artifact(None, out, OutputType::File, None)?;

    let inputs = indexset![ArtifactGroup::Artifact(
        BuildArtifact::testing_new(
            base.unpack_target_label().unwrap().dupe(),
            "input",
            ActionIndex::new(1),
        )
        .into()
    )];
    let outputs = indexset![declared.as_output()];

    let unregistered_action = SimpleUnregisteredAction::new(
        vec![],
        CategoryRef::new("fake_action").unwrap().to_owned(),
        None,
    );

    let key = actions.register(
        &DeferredHolderKey::Base(base.dupe()),
        inputs,
        outputs,
        unregistered_action.clone(),
        None,
    )?;

    assert_eq!(actions.testing_pending_action_keys(), vec![key]);
    assert_eq!(declared.testing_is_bound(), true);

    Ok(())
}

#[test]
fn finalizing_actions() -> anyhow::Result<()> {
    let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
        "cell//pkg:foo",
        ConfigurationData::testing_new(),
    ));
    let mut actions = ActionsRegistry::new(
        DeferredHolderKey::Base(base.dupe()),
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
            "input",
            ActionIndex::new(1),
        )
        .into()
    )];
    let outputs = indexset![declared.as_output()];

    let unregistered_action = SimpleUnregisteredAction::new(
        vec![],
        CategoryRef::new("fake_action").unwrap().to_owned(),
        None,
    );
    let holder_key = DeferredHolderKey::Base(base.dupe());
    actions.register(&holder_key, inputs, outputs, unregistered_action, None)?;

    let result = actions.ensure_bound(&AnalysisValueFetcher::testing_new(holder_key))?;

    assert_eq!(
        result
            .lookup(&declared.testing_action_key().unwrap())
            .is_ok(),
        true,
        "Expected results to contain `{}`, had `[{}]`",
        declared.testing_action_key().unwrap(),
        result.iter_actions().map(|v| v.key()).join(", ")
    );

    Ok(())
}

#[test]
fn duplicate_category_singleton_actions() {
    let result =
        category_identifier_test(&[("singleton_category", None), ("singleton_category", None)])
            .unwrap_err();

    assert!(
        result
            .category_key()
            .ends_with("ActionErrors::ActionCategoryDuplicateSingleton")
    );
}

#[test]
fn duplicate_category_identifier() {
    let result = category_identifier_test(&[
        ("cxx_compile", Some("foo.cpp")),
        ("cxx_compile", Some("foo.cpp")),
    ])
    .unwrap_err();

    assert!(
        result
            .category_key()
            .ends_with("ActionErrors::ActionCategoryIdentifierNotUnique")
    );
}

fn category_identifier_test(
    action_names: &[(&'static str, Option<&'static str>)],
) -> buck2_error::Result<()> {
    let base = DeferredHolderKey::testing_new("cell//pkg:foo");
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
            Category::new((*category).to_owned()).unwrap(),
            identifier.map(|i| i.to_owned()),
        );

        actions.register(&base, indexset![], indexset![], unregistered_action, None)?;
    }

    actions.ensure_bound(&AnalysisValueFetcher::testing_new(base))?;
    Ok(())
}
