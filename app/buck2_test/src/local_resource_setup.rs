/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::local_resource_info::FrozenLocalResourceInfo;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::ErrorTag;
use buck2_error::internal_error;
use buck2_test_api::data::RequiredLocalResources;
use buck2_test_api::data::TestStage;
use dice::DiceComputations;
use futures::FutureExt;
use itertools::Itertools;
use starlark::values::OwnedFrozenValueTyped;

pub(crate) enum TestStageSimple {
    Listing,
    Testing,
}

impl From<&TestStage> for TestStageSimple {
    fn from(value: &TestStage) -> Self {
        match value {
            TestStage::Listing { .. } => TestStageSimple::Listing,
            TestStage::Testing { .. } => TestStageSimple::Testing,
        }
    }
}

pub(crate) async fn required_providers<'v>(
    dice: &mut DiceComputations<'_>,
    test_info: &'v FrozenExternalRunnerTestInfo,
    required_local_resources: &'v RequiredLocalResources,
    stage: &TestStageSimple,
) -> buck2_error::Result<
    Vec<(
        &'v ConfiguredTargetLabel,
        OwnedFrozenValueTyped<FrozenLocalResourceInfo>,
    )>,
> {
    let available_resources = test_info.local_resources();

    let targets = required_local_resources
        .resources
        .iter()
        .map(|resource_type| &resource_type.name as &'v str)
        .chain(
            test_info
                .required_local_resources()
                .filter_map(|r| match stage {
                    TestStageSimple::Listing if r.listing => Some(&r.name as &str),
                    TestStageSimple::Testing if r.execution => Some(&r.name as &str),
                    _ => None,
                }),
        )
        .unique()
        .map(|type_name| {
            available_resources.get(type_name).copied().ok_or_else(|| {
                buck2_error::buck2_error!(
                    ErrorTag::Input,
                    "Required local resource of type `{type_name}` not found.",
                )
            })
        })
        .filter_map(|r| match r {
            Ok(Some(x)) => Some(Ok(x)),
            Ok(None) => None,
            Err(e) => {
                let _ignore = soft_error!("missing_required_local_resource", e, quiet: true);
                None
            }
        })
        .collect::<Result<Vec<_>, buck2_error::Error>>()?;

    dice.compute_join(targets, |dice, target| {
        async move { get_local_resource_info(dice, target).await }.boxed()
    })
    .await
    .into_iter()
    .collect::<Result<Vec<_>, _>>()
}

async fn get_local_resource_info<'v>(
    dice: &mut DiceComputations<'_>,
    target: &'v ConfiguredProvidersLabel,
) -> buck2_error::Result<(
    &'v ConfiguredTargetLabel,
    OwnedFrozenValueTyped<FrozenLocalResourceInfo>,
)> {
    let local_resource_info = dice
        .get_providers(target)
        .await?
        .require_compatible()?
        .value
        .maybe_map(|c| {
            c.as_ref()
                .builtin_provider_value::<FrozenLocalResourceInfo>()
        })
        .ok_or_else(|| {
            internal_error!("Target `{target}` expected to contain `LocalResourceInfo` provider")
        })?;
    Ok((target.target(), local_resource_info))
}
