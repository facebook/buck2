/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.

#![cfg(test)]

use std::borrow::Cow;

use async_trait::async_trait;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_query_parser::parse_expr;
use derive_more::Display;
use dupe::Dupe;

use crate::query::environment::QueryEnvironment;
use crate::query::environment::QueryTarget;
use crate::query::graph::node::LabeledNode;
use crate::query::graph::node::NodeKey;
use crate::query::graph::successors::AsyncChildVisitor;
use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::evaluator::QueryEvaluator;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::set::TargetSet;
use crate::query::syntax::simple::functions::DefaultQueryFunctionsModule;

#[derive(Clone, Hash, PartialEq, Eq, Debug, Display)]
struct TargetRef(String);

impl NodeKey for TargetRef {}

#[derive(Debug, Display)]
struct TargetAttr(String);

#[derive(Debug, Clone, Dupe, Eq, PartialEq)]
struct Target {}

impl LabeledNode for Target {
    type Key = TargetRef;

    fn node_key(&self) -> &Self::Key {
        unimplemented!()
    }
}

impl QueryTarget for Target {
    type Attr<'a> = TargetAttr;

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(&self, _func: F) -> Result<(), E> {
        unimplemented!()
    }

    fn rule_type(&self) -> Cow<str> {
        unimplemented!()
    }

    fn name(&self) -> Cow<str> {
        unimplemented!()
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        unimplemented!()
    }

    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Key> + Send + 'a> {
        unimplemented!()
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Key> + Send + 'a> {
        unimplemented!()
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Key> + Send + 'a> {
        unimplemented!()
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn attr_any_matches(
        _attr: &Self::Attr<'_>,
        _filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        unimplemented!()
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn defined_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, _key: &str, _func: F) -> R {
        unimplemented!()
    }
}

struct Env;
#[async_trait]
impl QueryEnvironment for Env {
    type Target = Target;

    async fn get_node(&self, _node_ref: &TargetRef) -> anyhow::Result<Self::Target> {
        unimplemented!()
    }

    async fn get_node_for_default_configured_target(
        &self,
        _node_ref: &TargetRef,
    ) -> anyhow::Result<MaybeCompatible<Self::Target>> {
        unimplemented!()
    }

    async fn eval_literals(&self, _literal: &[&str]) -> anyhow::Result<TargetSet<Self::Target>> {
        unimplemented!()
    }

    async fn eval_file_literal(&self, _literal: &str) -> anyhow::Result<FileSet> {
        unimplemented!()
    }

    async fn dfs_postorder(
        &self,
        _root: &TargetSet<Self::Target>,
        _delegate: impl AsyncChildVisitor<Self::Target>,
        _visit: impl FnMut(Self::Target) -> anyhow::Result<()> + Send,
    ) -> anyhow::Result<()> {
        unimplemented!()
    }

    async fn depth_limited_traversal(
        &self,
        _root: &TargetSet<Self::Target>,
        _delegate: impl AsyncChildVisitor<Self::Target>,
        _visit: impl FnMut(Self::Target) -> anyhow::Result<()> + Send,
        _depth: u32,
    ) -> anyhow::Result<()> {
        unimplemented!()
    }

    async fn owner(&self, _paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>> {
        unimplemented!()
    }
}

#[tokio::test]
pub async fn test_missing_arg() -> anyhow::Result<()> {
    let input = "kind(a, kind(a, kind()))";
    let parsed = parse_expr(input)?;
    match QueryEvaluator::new(&Env, &DefaultQueryFunctionsModule::new())
        .eval(&parsed)
        .await
    {
        Ok(_) => panic!(),
        Err(e) => {
            let err = QueryError::convert_error(e, input);
            let msg = format!("{:#}", err);
            let expected = "too few args. function `kind` requires at least ";
            if !msg.contains(expected) {
                return Err(err.context(format!("Expected error to contain `{}`", expected)));
            }
        }
    }
    Ok(())
}
