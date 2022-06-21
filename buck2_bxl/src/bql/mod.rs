/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Exposes all the bql functions as a starlark module.
//!
//! See <https://buck.build/command/query.html#allpaths> and <https://docs.bazel.build/versions/master/query.html> for rough semantics

use buck2_build_api::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSetExt;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::Value;
use starlark::values::ValueOf;

use crate::bql::internals::QueryInternals;
use crate::bql::internals::TargetExpr;
use crate::bql::values::fileset::FileSetExpr;
use crate::bql::values::fileset::StarlarkFileSet;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

pub mod eval;
pub mod internals;
pub mod values;

macro_rules! env {
    ($ctx:expr) => {
        QueryInternals::get($ctx)?.env()
    };
}

/// Converts a TargetExpr to a &TargetSet.
macro_rules! targets {
    ($ctx:expr, $e:expr) => {
        &*($e.get($ctx)?)
    };
}

// See https://buck.build/command/query.html#allpaths and https://docs.bazel.build/versions/master/query.html for rough semantics
#[starlark_module]
fn query_functions(builder: &mut GlobalsBuilder) {
    fn args<'v>(eval: &mut Evaluator<'v, '_>) -> anyhow::Result<Value<'v>> {
        Ok(QueryInternals::get(eval)?.args(eval.heap()))
    }

    fn allpaths<'v>(
        from: TargetExpr<'v>,
        to: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            QueryInternals::get(eval)?.allpaths(targets!(eval, from), targets!(eval, to))?,
        ))
    }

    fn somepath<'v>(
        from: TargetExpr<'v>,
        to: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            QueryInternals::get(eval)?.somepath(targets!(eval, from), targets!(eval, to))?,
        ))
    }

    fn attrfilter<'v>(
        attribute: &str,
        value: &str,
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).attrfilter(attribute, &|v| Ok(v == value))?,
        ))
    }

    fn nattrfilter<'v>(
        attribute: &str,
        value: &str,
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).nattrfilter(attribute, &|v| Ok(v == value))?,
        ))
    }

    fn attrregexfilter<'v>(
        attribute: &str,
        value: &str,
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).attrregexfilter(attribute, value)?,
        ))
    }

    fn buildfile<'v>(
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkFileSet> {
        Ok(StarlarkFileSet(targets!(eval, targets).buildfile()))
    }

    fn deps<'v>(
        targets: TargetExpr<'v>,
        depth: Option<i32>,
        filter: Option<&str>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(QueryInternals::get(eval)?.deps(
            targets!(eval, targets),
            depth,
            filter,
        )?))
    }

    fn filter_name<'v>(
        regex: &str,
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).filter_name(regex)?,
        ))
    }

    fn inputs<'v>(
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkFileSet> {
        Ok(StarlarkFileSet(targets!(eval, targets).inputs()?))
    }

    fn kind<'v>(
        regex: &str,
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(targets!(eval, targets).kind(regex)?))
    }

    #[allow(unused_variables)]
    fn labels<'v>(attr: &str, targets: TargetExpr<'v>) -> anyhow::Result<Value<'v>> {
        unimplemented!()
    }

    fn owner<'v>(
        files: FileSetExpr,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(files.get(eval)?.owner(env!(eval))?))
    }

    fn rdeps<'v>(
        universe: TargetExpr<'v>,
        from: TargetExpr<'v>,
        depth: Option<i32>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(QueryInternals::get(eval)?.rdeps(
            targets!(eval, universe),
            targets!(eval, from),
            depth,
        )?))
    }

    fn testsof<'v>(
        targets: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            QueryInternals::get(eval)?.testsof(targets!(eval, targets))?,
        ))
    }

    fn targets<'v>(
        val: ValueOf<'v, TargetExpr<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        QueryInternals::get(eval)?.targets(eval, val)
    }

    fn union<'v>(
        left: TargetExpr<'v>,
        right: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, left).union(targets!(eval, right)),
        ))
    }

    fn intersect<'v>(
        left: TargetExpr<'v>,
        right: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, left).intersect(targets!(eval, right))?,
        ))
    }

    fn difference<'v>(
        left: TargetExpr<'v>,
        right: TargetExpr<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, left).difference(targets!(eval, right))?,
        ))
    }
}

pub fn register_query_functions(registry: &mut GlobalsBuilder) {
    query_functions(registry)
}
