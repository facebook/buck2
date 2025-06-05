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
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use dice::DiceComputations;
use gazebo::prelude::OptionExt;
use starlark::values::Heap;
use starlark::values::Value;

use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::target_list_expr::OwnedTargetNodeArg;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::uquery::get_uquery_env;

#[derive(Debug, Allocative)]
pub(crate) enum LazyUqueryOperation {
    TestsOf(OwnedTargetNodeArg),
    AllPaths {
        from: OwnedTargetNodeArg,
        to: OwnedTargetNodeArg,
        filter: Option<String>,
    },
    SomePath {
        from: OwnedTargetNodeArg,
        to: OwnedTargetNodeArg,
        filter: Option<String>,
    },
}

pub(crate) enum LazyUqueryResult {
    TestsOf(StarlarkTargetSet<TargetNode>),
    AllPaths(StarlarkTargetSet<TargetNode>),
    SomePath(StarlarkTargetSet<TargetNode>),
}

impl LazyUqueryResult {
    pub(crate) fn into_value<'v>(self, heap: &'v Heap) -> buck2_error::Result<Value<'v>> {
        match self {
            LazyUqueryResult::TestsOf(target_set) => Ok(heap.alloc(target_set)),
            LazyUqueryResult::AllPaths(target_set) => Ok(heap.alloc(target_set)),
            LazyUqueryResult::SomePath(target_set) => Ok(heap.alloc(target_set)),
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
            LazyUqueryOperation::AllPaths { from, to, filter } => {
                let from = from.to_unconfigured_target_set(core_data, dice).await?;
                let to = to.to_unconfigured_target_set(core_data, dice).await?;
                let filter = filter
                    .as_ref()
                    .try_map(|s| buck2_query_parser::parse_expr(s.as_str()))?;
                let expr = filter.as_ref().map(|expr| CapturedExpr { expr });

                let res = get_uquery_env(core_data)
                    .await?
                    .allpaths(dice, &from, &to, expr.as_ref())
                    .await?;

                Ok(LazyUqueryResult::AllPaths(StarlarkTargetSet::from(res)))
            }
            LazyUqueryOperation::SomePath { from, to, filter } => {
                let from = from.to_unconfigured_target_set(core_data, dice).await?;
                let to = to.to_unconfigured_target_set(core_data, dice).await?;
                let filter = filter
                    .as_ref()
                    .try_map(|s| buck2_query_parser::parse_expr(s.as_str()))?;
                let expr = filter.as_ref().map(|expr| CapturedExpr { expr });

                let res = get_uquery_env(core_data)
                    .await?
                    .somepath(dice, &from, &to, expr.as_ref())
                    .await?;

                Ok(LazyUqueryResult::SomePath(StarlarkTargetSet::from(res)))
            }
        }
    }
}
