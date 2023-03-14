/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;

use buck2_core::cells::CellResolver;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::target::label::TargetLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::query::aquery::environment::ActionQueryNode;

/// The result of audit output.
pub enum AuditOutputResult {
    /// The exact action that matched the buck-out path.
    Match(ActionQueryNode),
    /// If the platform configuration of the buck-out path doesn't match the platform used when calling
    /// audit output, then we return the unconfigured target label.
    MaybeRelevant(TargetLabel),
}

pub static AUDIT_OUTPUT: LateBinding<
    for<'v> fn(
        &'v str,
        &'v ProjectRelativePath,
        &'v CellResolver,
        &'v DiceComputations,
        global_target_platform: Option<TargetLabel>,
    )
        -> Pin<Box<dyn Future<Output = anyhow::Result<Option<AuditOutputResult>>> + 'v>>,
> = LateBinding::new("AUDIT_OUTPUT");

pub async fn audit_output<'v>(
    output_path: &'v str,
    working_dir: &'v ProjectRelativePath,
    cell_resolver: &'v CellResolver,
    dice_ctx: &'v DiceComputations,
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<Option<AuditOutputResult>> {
    (AUDIT_OUTPUT.get()?)(
        output_path,
        working_dir,
        cell_resolver,
        dice_ctx,
        global_target_platform,
    )
    .await
}
