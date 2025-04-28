/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_node::nodes::unconfigured::TargetNode;
use dice::DiceComputations;
use starlark::values::Heap;
use starlark::values::Value;

use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::target_list_expr::OwnedTargetNodeArg;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::uquery::get_uquery_env;

#[derive(Debug, Allocative)]
pub(crate) enum LazyUqueryOperation {
    TestsOf(OwnedTargetNodeArg),
}

pub(crate) enum LazyUqueryResult {
    TestsOf(StarlarkTargetSet<TargetNode>),
}

impl LazyUqueryResult {
    pub(crate) fn into_value<'v>(self, heap: &'v Heap) -> buck2_error::Result<Value<'v>> {
        match self {
            LazyUqueryResult::TestsOf(target_set) => Ok(heap.alloc(target_set)),
        }
    }
}

impl LazyUqueryOperation {
    pub(crate) async fn resolve(
        &self,
        dice: &mut DiceComputations<'_>,
        core_data: &BxlContextCoreData,
    ) -> buck2_error::Result<LazyUqueryResult> {
        match self {
            LazyUqueryOperation::TestsOf(expr) => {
                let target_set = expr.to_unconfigured_target_set(core_data, dice).await?;

                let res = get_uquery_env(core_data)
                    .await?
                    .testsof(dice, &target_set)
                    .await?;

                Ok(LazyUqueryResult::TestsOf(StarlarkTargetSet::from(res)))
            }
        }
    }
}
