/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::anyhow;
use buck2_query::query::{environment::QueryEnvironment, syntax::simple::eval::set::TargetSet};
use gazebo::{any::ProvidesStaticType, prelude::*};
use starlark::{
    eval::Evaluator,
    values::{list::List, Heap, UnpackValue, Value, ValueLike, ValueOf},
};
use tokio::runtime::Handle;

use crate::{bxl::starlark_defs::targetset::StarlarkTargetSet, nodes::unconfigured::TargetNode};

/// QueryInternals is added as part of the Starlark context's extra information.
/// Anything we need to implement the starlark-exposed functions will be
/// available from the internals.
#[derive(ProvidesStaticType)]
pub struct QueryInternals<'c> {
    env: Arc<dyn QueryEnvironment<Target = TargetNode> + 'c>,
    handle: Handle,
    args: Vec<String>,
}

impl<'c> QueryInternals<'c> {
    pub fn new(
        handle: Handle,
        env: Arc<dyn QueryEnvironment<Target = TargetNode> + 'c>,
        args: Vec<String>,
    ) -> Self {
        Self { env, handle, args }
    }

    pub(crate) fn eval_literals_sync(
        &self,
        literal: &[&str],
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        self.handle.block_on(self.env.eval_literals(literal))
    }

    pub fn get<'a>(ctx: &'a Evaluator<'_, 'c>) -> anyhow::Result<&'a Self> {
        ctx.extra
            .ok_or_else(|| anyhow!("should have had a QueryInternals context extra!"))?
            .downcast_ref::<QueryInternals>()
            .ok_or_else(|| anyhow!("should have had a QueryInternals context extra!"))
    }

    pub fn env(&self) -> &dyn QueryEnvironment<Target = TargetNode> {
        &*self.env
    }

    pub fn args<'v>(&self, heap: &'v Heap) -> Value<'v> {
        let as_vals: Vec<Value> = self.args.map(|v| heap.alloc(v));
        heap.alloc(as_vals)
    }

    pub fn targets<'v>(
        &self,
        eval: &Evaluator<'v, '_>,
        expr: ValueOf<'v, TargetExpr<'v>>,
    ) -> anyhow::Result<Value<'v>> {
        match expr.typed {
            TargetExpr::Literal(val) => TargetExpr::resolve_literal(eval, val),
            TargetExpr::Iterable(val) => TargetExpr::resolve_iterable(eval, val),
            TargetExpr::TargetSet(_) => Ok(expr.value),
        }
    }

    pub fn deps(
        &self,
        targets: &TargetSet<TargetNode>,
        depth: Option<i32>,
        _filter: Option<&str>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        self.handle
            .block_on(async move { self.env.deps(targets, depth, None).await })
    }

    pub fn rdeps(
        &self,
        universe: &TargetSet<TargetNode>,
        targets: &TargetSet<TargetNode>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        self.handle
            .block_on(async move { self.env.rdeps(universe, targets, depth).await })
    }

    pub fn testsof(
        &self,
        targets: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        self.handle
            .block_on(async move { self.env.testsof(targets).await })
    }

    pub fn somepath(
        &self,
        from: &TargetSet<TargetNode>,
        to: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        self.handle
            .block_on(async move { self.env.somepath(from, to).await })
    }

    pub fn allpaths(
        &self,
        from: &TargetSet<TargetNode>,
        to: &TargetSet<TargetNode>,
    ) -> anyhow::Result<TargetSet<TargetNode>> {
        self.handle
            .block_on(async move { self.env.allpaths(from, to).await })
    }
}

/// TargetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be target sets. It will accept a
/// literal (like `//some:target`) or list of literals or a TargetSet Value (from one of the
/// bql functions that return them). It can be resolved to a `&TargetSet` with
/// the help of the `tgts!()` macro.
pub enum TargetExpr<'v> {
    Literal(&'v str),
    Iterable(Value<'v>),
    TargetSet(&'v StarlarkTargetSet<TargetNode>),
}

// TODO(cjhopman): This is a bit awkward due to the fact that we need to return
// a Value from get() because we need the caller to keep it alive for the
// lifetime of the ARef from the downcast_ref in as_target_set().
impl<'v> TargetExpr<'v> {
    pub fn get(
        self,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<&'v StarlarkTargetSet<TargetNode>> {
        match self {
            TargetExpr::Literal(val) => {
                Ok(StarlarkTargetSet::from_value(Self::resolve_literal(eval, val)?).unwrap())
            }
            TargetExpr::Iterable(val) => {
                Ok(StarlarkTargetSet::from_value(Self::resolve_iterable(eval, val)?).unwrap())
            }
            TargetExpr::TargetSet(val) => Ok(val),
        }
    }

    fn resolve_literal<'u>(eval: &Evaluator<'u, '_>, val: &str) -> anyhow::Result<Value<'u>> {
        let targets = QueryInternals::get(eval)?.eval_literals_sync(&[val])?;
        Ok(eval.heap().alloc(StarlarkTargetSet(targets)))
    }

    fn resolve_iterable<'u>(eval: &Evaluator<'u, '_>, val: Value<'u>) -> anyhow::Result<Value<'u>> {
        let mut targets = Vec::new();
        // This is verified as a list of strings in unpack_set.
        for val in List::from_value(val).unwrap().iter() {
            targets.push(val.unpack_str().unwrap());
        }
        let targets = QueryInternals::get(eval)?.eval_literals_sync(&targets)?;
        Ok(eval.heap().alloc(StarlarkTargetSet(targets)))
    }

    // This will unpack a Value to a TargetExpr, but doesn't accept as single string literal,
    // only a TargetSet or a list of string literals.
    fn unpack_set(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            return Some(TargetExpr::TargetSet(s));
        } else if let Some(iterable) = List::from_value(value) {
            let mut good = true;
            for val in iterable.iter() {
                if val.unpack_str().is_none() {
                    good = false;
                    break;
                }
            }
            if good {
                return Some(TargetExpr::Iterable(value));
            }
        }
        None
    }
}

impl<'v> UnpackValue<'v> for TargetExpr<'v> {
    fn expected() -> String {
        "str or target set".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(TargetExpr::Literal(s))
        } else {
            TargetExpr::unpack_set(value)
        }
    }
}
