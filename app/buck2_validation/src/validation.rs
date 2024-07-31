/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::provider::builtin::validation_info::FrozenValidationInfo;
use buck2_build_api::materialize::materialize_artifact_group;
use buck2_build_api::materialize::MaterializationContext;
use buck2_build_api::validation_impl::ValidationImpl;
use buck2_build_api::validation_impl::VALIDATION_IMPL;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use dice::DiceComputations;
use dice::LinearRecomputeDiceComputations;
use dupe::Dupe;

use crate::parse_validation_result_key::CachedValidationResult;
use crate::parse_validation_result_key::CachedValidationResultData;
use crate::parse_validation_result_key::ParseValidationResultKey;

pub(crate) fn init_validation_impl() {
    VALIDATION_IMPL.init(&ValidationImplInstance);
}

struct ValidationImplInstance;

#[async_trait]
impl ValidationImpl for ValidationImplInstance {
    async fn validate_target_node_transitively<'a>(
        &self,
        ctx: &'a LinearRecomputeDiceComputations<'_>,
        materialization_context: &'a MaterializationContext,
        target_node: ConfiguredTargetNode,
    ) -> Result<(), buck2_error::Error> {
        validate_target_node_transitively(ctx, materialization_context, target_node).await
    }
}

#[derive(buck2_error::Error, Debug)]
#[error(
    "Validation for `{target}` failed:\n\n{message}.\n\nFull validation result is located at: `{result_path}`"
)]
struct ValidationFailedUserFacingError {
    target: BaseDeferredKey,
    message: String,
    result_path: AbsNormPathBuf,
}

async fn validate_target_node_transitively<'a>(
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    materialization_context: &'a MaterializationContext,
    target_node: ConfiguredTargetNode,
) -> Result<(), buck2_error::Error> {
    let validation_outputs =
        collect_validation_result_artifacts(&mut ctx.get(), &target_node).await?;
    validate_all(ctx, materialization_context, validation_outputs).await
}

async fn collect_validation_result_artifacts(
    ctx: &mut DiceComputations<'_>,
    root_node: &ConfiguredTargetNode,
) -> anyhow::Result<Vec<Artifact>> {
    let mut result = Vec::new();
    let mut stack = vec![root_node];
    let mut visited = LabelIndexedSet::new();
    while let Some(node) = stack.pop() {
        if !visited.insert(node.dupe()) {
            continue;
        }
        // We do not parallelize fetching providers here, because at this point we are already past
        // the analysis stage and this call should not induce any DICE computations.
        let providers = ctx
            .get_providers(&ConfiguredProvidersLabel::default_for(node.label().dupe()))
            .await?;
        match providers {
            MaybeCompatible::Compatible(providers) => {
                if let Some(provider) = providers
                    .provider_collection()
                    .builtin_provider::<FrozenValidationInfo>()
                {
                    for spec in provider.as_ref().validations() {
                        result.push(spec.validation_result().get_bound_artifact()?);
                    }
                }
            }
            MaybeCompatible::Incompatible(_reason) => {
                return Err(internal_error!("Incompatible node is not expected"));
            }
        }
        stack.extend(node.deps());
    }
    Ok(result)
}

async fn validate_all<'a>(
    ctx: &'a LinearRecomputeDiceComputations<'_>,
    materialization_context: &'a MaterializationContext,
    validation_results: Vec<Artifact>,
) -> buck2_error::Result<()> {
    let futures = validation_results.into_iter().map({
        move |output| async move {
            let res = validate_single_node(&mut ctx.get(), &materialization_context, &output).await;
            match res {
                Ok(validation_result) => match validation_result.0.as_ref() {
                    CachedValidationResultData::Success => Ok(()),
                    CachedValidationResultData::Failure {
                        short_message,
                        validation_result_path,
                    } => {
                        let validation_result_path = {
                            let fs = ctx.get().get_artifact_fs().await?;
                            let project_relative_path = fs
                                .buck_out_path_resolver()
                                .resolve_gen(validation_result_path);
                            fs.fs().resolve(&project_relative_path)
                        };
                        Err(buck2_error::Error::from(ValidationFailedUserFacingError {
                            target: output
                                .owner()
                                .internal_error("Expected validation to be a build artifact")?
                                .dupe(),
                            message: short_message.clone(),
                            result_path: validation_result_path,
                        }))
                    }
                },
                Err(e) => Err(e.into()),
            }
        }
    });
    buck2_util::future::try_join_all(futures).await?;
    Ok(())
}

pub(crate) async fn validate_single_node(
    ctx: &mut DiceComputations<'_>,
    materialization_context: &MaterializationContext,
    validation_result: &Artifact,
) -> anyhow::Result<CachedValidationResult> {
    // Make sure validation result is materialized before we parse it
    materialize_artifact_group(
        ctx,
        &ArtifactGroup::Artifact(validation_result.dupe()),
        materialization_context,
    )
    .await?;
    let action_key = validation_result
        .action_key()
        .internal_error("Validation result must be a build artifact")?;
    ctx.compute(&ParseValidationResultKey(action_key.dupe()))
        .await?
        .map_err(anyhow::Error::from)
}
