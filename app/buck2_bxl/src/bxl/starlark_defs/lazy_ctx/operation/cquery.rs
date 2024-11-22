/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_recursion::async_recursion;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_error::starlark_error::from_starlark_with_options;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationValue;
use dice::DiceComputations;
use indexmap::IndexMap;
use starlark::values::dict::Dict;
use starlark::values::Heap;
use starlark::values::Value;

use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::file_set::StarlarkFileSet;
use crate::bxl::starlark_defs::targetset::NodeLike;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

#[derive(Debug, Allocative)]
pub(crate) struct LazyCqueryOperation {
    global_cfg_options: GlobalCfgOptions,
    args: LazyCqueryArg,
}

#[derive(Debug, Allocative)]
pub(crate) enum LazyCqueryArg {
    Eval {
        query: String,
        query_args: Vec<String>,
        target_universe: Option<Vec<String>>,
    },
}

impl LazyCqueryOperation {
    pub(crate) fn new_eval(
        global_cfg_options: GlobalCfgOptions,
        query: String,
        query_args: Vec<String>,
        target_universe: Option<Vec<String>>,
    ) -> Self {
        Self {
            global_cfg_options,
            args: LazyCqueryArg::Eval {
                query,
                query_args,
                target_universe,
            },
        }
    }
}

pub(crate) enum SingleOrMappedQueryEvaluationValue<T: QueryTarget> {
    Single(QueryEvaluationValue<T>),
    Map(IndexMap<String, QueryEvaluationValue<T>>),
}

impl<T: NodeLike> SingleOrMappedQueryEvaluationValue<T> {
    pub(crate) fn from_query_evaluation_result(
        result: QueryEvaluationResult<T>,
    ) -> buck2_error::Result<Self> {
        match result {
            QueryEvaluationResult::Single(v) => Ok(Self::Single(v)),
            QueryEvaluationResult::Multiple(multi) => Ok(Self::Map(
                multi
                    .0
                    .into_iter()
                    .map(|(key, res)| res.map(|val| (key, val)))
                    .collect::<buck2_error::Result<IndexMap<_, _>>>()?,
            )),
        }
    }

    pub(crate) fn into_value<'v>(self, heap: &'v Heap) -> buck2_error::Result<Value<'v>> {
        Ok(match self {
            SingleOrMappedQueryEvaluationValue::Single(res) => match res {
                QueryEvaluationValue::TargetSet(targets) => {
                    heap.alloc(StarlarkTargetSet::from(targets))
                }
                QueryEvaluationValue::FileSet(files) => heap.alloc(StarlarkFileSet::from(files)),
            },
            SingleOrMappedQueryEvaluationValue::Map(map) => heap.alloc(Dict::new(
                map.into_iter()
                    .map(|(q, val)| {
                        Ok((
                            heap.alloc(q).get_hashed().map_err(|e| {
                                from_starlark_with_options(
                                    e,
                                    buck2_error::starlark_error::NativeErrorHandling::Unknown,
                                    false,
                                )
                            })?,
                            match val {
                                QueryEvaluationValue::TargetSet(targets) => {
                                    heap.alloc(StarlarkTargetSet::from(targets))
                                }
                                QueryEvaluationValue::FileSet(files) => {
                                    heap.alloc(StarlarkFileSet::from(files))
                                }
                            },
                        ))
                    })
                    .collect::<buck2_error::Result<_>>()?,
            )),
        })
    }
}

pub(crate) enum LazyCqueryResult {
    Eval(SingleOrMappedQueryEvaluationValue<ConfiguredTargetNode>),
}

impl LazyCqueryOperation {
    #[async_recursion]
    pub(crate) async fn resolve(
        &self,
        dice: &mut DiceComputations<'_>,
        core_data: &BxlContextCoreData,
    ) -> buck2_error::Result<LazyCqueryResult> {
        match &self.args {
            LazyCqueryArg::Eval {
                query,
                query_args,
                target_universe,
            } => {
                let res = QUERY_FRONTEND
                    .get()?
                    .eval_cquery(
                        dice,
                        &core_data.working_dir()?,
                        query,
                        query_args,
                        self.global_cfg_options.clone(),
                        target_universe.as_ref().map(|items| &items[..]),
                        false,
                    )
                    .await?
                    .0;
                Ok(LazyCqueryResult::Eval(
                    SingleOrMappedQueryEvaluationValue::from_query_evaluation_result(res)?,
                ))
            }
        }
    }
}

impl LazyCqueryResult {
    pub(crate) fn into_value<'v>(self, heap: &'v Heap) -> buck2_error::Result<Value<'v>> {
        match self {
            LazyCqueryResult::Eval(result) => Ok(result.into_value(heap)?),
        }
    }
}
