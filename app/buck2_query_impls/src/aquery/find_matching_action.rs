/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any;

use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::artifact::provide_outputs::ProvideOutputs;
use buck2_build_api::actions::key::ActionKey;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::actions::query::FIND_MATCHING_ACTION;
use buck2_build_api::analysis::AnalysisResult;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::target::label::TargetLabel;
use dice::DiceComputations;
use tracing::debug;

use crate::aquery::evaluator::get_dice_aquery_delegate;

fn check_output_path<'v>(
    build_artifact: &'v BuildArtifact,
    path_to_check: &'v ForwardRelativePathBuf,
) -> anyhow::Result<Option<&'v ActionKey>> {
    let path = build_artifact.get_path().path();

    debug!(
        "Checking action's output path: {}. Path to check: {}",
        path, path_to_check
    );

    if path_to_check.starts_with(path_to_check) {
        Ok(Some(build_artifact.key()))
    } else {
        Ok(None)
    }
}

async fn find_matching_action(
    ctx: &DiceComputations,
    working_dir: &ProjectRelativePath,
    global_target_platform: Option<TargetLabel>,
    analysis: &AnalysisResult,
    path_after_target_name: ForwardRelativePathBuf,
) -> anyhow::Result<Option<ActionQueryNode>> {
    let dice_aquery_delegate =
        get_dice_aquery_delegate(ctx, working_dir, global_target_platform.clone()).await?;

    for entry in analysis.iter_deferreds() {
        match any::request_value::<ProvideOutputs>(entry.as_complex()) {
            Some(outputs) => {
                let outputs = outputs.0?;
                for build_artifact in &outputs {
                    match check_output_path(build_artifact, &path_after_target_name)? {
                        Some(action_key) => {
                            return Ok(Some(
                                dice_aquery_delegate.get_action_node(action_key).await?,
                            ));
                        }
                        None => (),
                    }
                }
            }
            None => debug!("Could not extract outputs from deferred table entry"),
        }
    }
    Ok(None)
}

pub(crate) fn init_find_matching_action() {
    FIND_MATCHING_ACTION.init(
        |ctx, working_dir, global_target_platform, analysis, path_after_target_name| {
            Box::pin(find_matching_action(
                ctx,
                working_dir,
                global_target_platform,
                analysis,
                path_after_target_name,
            ))
        },
    );
}
