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

use buck2_query::query::syntax::simple::eval::set::TargetSetExt;
use starlark::{
    environment::GlobalsBuilder,
    values::{Value, ValueOf},
};

use crate::{
    bql::{
        internals::{QueryInternals, TargetExpr},
        values::fileset::{FileSetExpr, StarlarkFileSet},
    },
    bxl::StarlarkTargetSet,
    nodes::unconfigured::TargetNode,
};

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
    fn args<'v>() -> anyhow::Result<Value<'v>> {
        Ok(QueryInternals::get(eval)?.args(heap))
    }

    fn allpaths(from: TargetExpr, to: TargetExpr) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            QueryInternals::get(eval)?.allpaths(targets!(eval, from), targets!(eval, to))?,
        ))
    }

    fn somepath(from: TargetExpr, to: TargetExpr) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            QueryInternals::get(eval)?.somepath(targets!(eval, from), targets!(eval, to))?,
        ))
    }

    fn attrfilter(
        attribute: &str,
        value: &str,
        targets: TargetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).attrfilter(attribute, &|v| Ok(v == value))?,
        ))
    }

    fn nattrfilter(
        attribute: &str,
        value: &str,
        targets: TargetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).nattrfilter(attribute, &|v| Ok(v == value))?,
        ))
    }

    fn attrregexfilter(
        attribute: &str,
        value: &str,
        targets: TargetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).attrregexfilter(attribute, value)?,
        ))
    }

    fn buildfile(targets: TargetExpr) -> anyhow::Result<StarlarkFileSet> {
        Ok(StarlarkFileSet(targets!(eval, targets).buildfile()?))
    }

    fn deps(
        targets: TargetExpr,
        depth: Option<i32>,
        filter: Option<&str>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(QueryInternals::get(eval)?.deps(
            targets!(eval, targets),
            depth,
            filter,
        )?))
    }

    fn filter_name(
        regex: &str,
        targets: TargetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, targets).filter_name(regex)?,
        ))
    }

    fn inputs(targets: TargetExpr) -> anyhow::Result<StarlarkFileSet> {
        Ok(StarlarkFileSet(targets!(eval, targets).inputs()?))
    }

    fn kind(regex: &str, targets: TargetExpr) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(targets!(eval, targets).kind(regex)?))
    }

    #[allow(unused_variables)]
    fn labels<'v>(attr: &str, targets: TargetExpr) -> anyhow::Result<Value<'v>> {
        unimplemented!()
    }

    fn owner(files: FileSetExpr) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(files.get(eval)?.owner(env!(eval))?))
    }

    fn rdeps(
        universe: TargetExpr,
        from: TargetExpr,
        depth: Option<i32>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(QueryInternals::get(eval)?.rdeps(
            targets!(eval, universe),
            targets!(eval, from),
            depth,
        )?))
    }

    fn testsof(targets: TargetExpr) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            QueryInternals::get(eval)?.testsof(targets!(eval, targets))?,
        ))
    }

    fn targets<'v>(val: ValueOf<TargetExpr>) -> anyhow::Result<Value<'v>> {
        QueryInternals::get(eval)?.targets(eval, val)
    }

    fn union(left: TargetExpr, right: TargetExpr) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, left).union(targets!(eval, right))?,
        ))
    }

    fn intersect(
        left: TargetExpr,
        right: TargetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, left).intersect(targets!(eval, right))?,
        ))
    }

    fn difference(
        left: TargetExpr,
        right: TargetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet(
            targets!(eval, left).difference(targets!(eval, right))?,
        ))
    }
}

pub fn register_query_functions(registry: &mut GlobalsBuilder) {
    query_functions(registry)
}
