/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationValue;
use starlark::eval::Evaluator;
use starlark::values::dict::Dict;
use starlark::values::Value;

use super::targetset::NodeLike;
use crate::bxl::starlark_defs::file_set::StarlarkFileSet;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

pub(crate) fn parse_query_evaluation_result<'v, Env: QueryEnvironment>(
    result: QueryEvaluationResult<Env::Target>,
    eval: &mut Evaluator<'v, '_>,
) -> anyhow::Result<Value<'v>>
where
    <Env as QueryEnvironment>::Target: NodeLike,
{
    Ok(match result {
        QueryEvaluationResult::Single(result) => match result {
            QueryEvaluationValue::TargetSet(targets) => {
                eval.heap().alloc(StarlarkTargetSet::from(targets))
            }
            QueryEvaluationValue::FileSet(files) => eval.heap().alloc(StarlarkFileSet::from(files)),
        },
        QueryEvaluationResult::Multiple(multi) => eval.heap().alloc(Dict::new(
            multi
                .0
                .into_iter()
                .map(|(q, res)| {
                    Ok((
                        eval.heap().alloc(q).get_hashed()?,
                        match res? {
                            QueryEvaluationValue::TargetSet(targets) => {
                                eval.heap().alloc(StarlarkTargetSet::from(targets))
                            }
                            QueryEvaluationValue::FileSet(files) => {
                                eval.heap().alloc(StarlarkFileSet::from(files))
                            }
                        },
                    ))
                })
                .collect::<anyhow::Result<_>>()?,
        )),
    })
}
